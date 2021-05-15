const std = @import("std");
const Allocator = std.mem.Allocator;

const Env = @import("env.zig").Env;

pub const MalType = union(enum) {
    pub const Number = i32;
    pub const Symbol = []const u8;
    pub const String = []const u8;
    pub const Atom = union(enum) {
        t,
        f,
        nil,
        number: Number,
        // TODO: keyword
        string: String,
        symbol: Symbol,
    };
    pub const List = std.ArrayList(MalType);

    pub const TypeError = error{
        NotAtom,
        NotNumber,
        NotSymbol,
        NotList,
    };

    atom: Atom,
    list: List,

    pub fn makeNumber(num: Number) MalType {
        return .{ .atom = .{ .number = num } };
    }

    pub fn makeString(string: []const u8) MalType {
        return .{ .atom = .{ .string = string } };
    }

    pub fn makeSymbol(symbol: []const u8) MalType {
        return .{ .atom = .{ .symbol = symbol } };
    }

    pub fn initList(list: List) MalType {
        return .{ .list = list };
    }

    pub fn initListAlloc(allocator: Allocator) !MalType {
        return MalType{ .list = try List.init(allocator) };
    }

    pub fn initListCapacity(allocator: Allocator, num: usize) !MalType {
        return MalType{ .list = try List.initCapacity(allocator, num) };
    }

    const Self = @This();

    pub fn deinit(self: Self, allocator: *Allocator) void {
        switch (self) {
            .list => |list| {
                for (list.items) |item| {
                    item.deinit(allocator);
                }
                list.deinit();
            },
            .atom => |atom| switch (atom) {
                .string, .symbol => |string_or_symbol| allocator.free(string_or_symbol),
                else => {},
            },
        }
    }

    pub fn copy(self: Self, allocator: *Allocator) Allocator.Error!MalType {
        return switch (self) {
            .list => |list| blk: {
                var list_copy = try List.initCapacity(allocator, list.items.len);
                for (list.items) |item| {
                    list_copy.appendAssumeCapacity(try item.copy(allocator));
                }
                break :blk .{ .list = list_copy };
            },
            .atom => |atom| switch (atom) {
                .string => |string| makeString(try allocator.dupe(u8, string)),
                .symbol => |symbol| makeSymbol(try allocator.dupe(u8, symbol)),
                else => self,
            },
        };
    }

    pub fn asAtom(self: Self) !Atom {
        return switch (self) {
            .atom => |atom| atom,
            else => error.NotAtom,
        };
    }

    pub fn asNumber(self: Self) !Number {
        return switch (self) {
            .atom => |atom| switch (atom) {
                .number => |number| number,
                else => error.NotNumber,
            },
            else => error.NotNumber,
        };
    }

    pub fn asSymbol(self: Self) !Symbol {
        return switch (self) {
            .atom => |atom| switch (atom) {
                .symbol => |symbol| symbol,
                else => error.NotSymbol,
            },
            else => error.NotSymbol,
        };
    }

    pub fn asList(self: Self) !List {
        return switch (self) {
            .list => |list| list,
            else => error.NotList,
        };
    }
};

pub const MalValue = union(enum) {
    pub const List = std.ArrayList(MalValue);
    pub const Function = union(enum) {
        const Parameters = std.ArrayList(MalType.Symbol);

        op_2_number: fn (a: MalType.Number, b: MalType.Number) MalType.Number,
        closure: struct {
            parameters: Parameters,
            body: MalType,
            env: *Env,
        },
    };

    pub const EvalError = error{
        NotNumber,
        NotFunction,
    };

    mal_type: MalType,
    list: List,
    function: Function,

    pub fn initListCapacity(allocator: Allocator, num: usize) !MalValue {
        return MalValue{ .list = try List.initCapacity(allocator, num) };
    }

    pub fn makeFunction(fn_ptr: anytype) MalValue {
        const type_info = @typeInfo(@TypeOf(fn_ptr));
        std.debug.assert(type_info == .Fn);
        return MalValue{ .function = switch (type_info.Fn.args.len) {
            2 => .{ .op_2_number = fn_ptr },
            else => unreachable,
        } };
    }

    pub fn makeString(string: []const u8) MalValue {
        return MalValue{ .mal_type = .{ .atom = .{ .string = string } } };
    }

    const Self = @This();

    pub fn deinit(self: Self, allocator: Allocator) void {
        switch (self) {
            .mal_type => |mal_type| mal_type.deinit(allocator),
            .list => |list| {
                for (list.items) |item| {
                    item.deinit(allocator);
                }
                list.deinit();
            },
            .function => |function| switch (function) {
                .closure => |closure| {
                    for (closure.parameters.items) |parameter| {
                        allocator.free(parameter);
                    }
                    closure.parameters.deinit();
                    closure.body.deinit(allocator);
                    closure.env.deinitAlloc(allocator);
                },
                else => {},
            },
        }
    }

    pub fn copy(self: Self, allocator: *Allocator) Allocator.Error!MalValue {
        return switch (self) {
            .mal_type => |mal_type| MalValue{ .mal_type = try mal_type.copy(allocator) },
            .list => |list| blk: {
                var list_copy = try List.initCapacity(allocator, list.items.len);
                for (list.items) |item| {
                    list_copy.appendAssumeCapacity(try item.copy(allocator));
                }
                break :blk MalValue{ .list = list_copy };
            },
            .function => |function| switch (function) {
                .closure => |closure| blk: {
                    var parameters_copy = try Function.Parameters.initCapacity(allocator, closure.parameters.items.len);
                    for (closure.parameters.items) |item| {
                        parameters_copy.appendAssumeCapacity(try std.mem.dupe(allocator, u8, item));
                    }
                    break :blk MalValue{ .function = .{ .closure = .{
                        .parameters = parameters_copy,
                        .body = try closure.body.copy(allocator),
                        .env = try closure.env.copy(allocator),
                    } } };
                },
                else => self,
            },
        };
    }

    pub fn isString(self: Self) bool {
        return self == .mal_type and self.mal_type == .atom and self.mal_type.atom == .string;
    }

    pub fn getString(self: Self) ?MalType.String {
        return if (self.isString()) self.mal_type.atom.string else null;
    }

    pub fn asNumber(self: Self) !MalType.Number {
        return switch (self) {
            .mal_type => |mal_type| switch (mal_type) {
                .atom => |atom| switch (atom) {
                    .number => |number| number,
                    else => error.NotNumber,
                },
                else => error.NotNumber,
            },
            else => error.NotNumber,
        };
    }

    pub fn asFunction(self: Self) !Function {
        return switch (self) {
            .function => |function| function,
            else => error.NotFunction,
        };
    }
};

fn copyList(comptime T: type, src_list: std.ArrayList(T)) std.ArrayList(T) {}
