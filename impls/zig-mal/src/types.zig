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

    pub fn makeBool(b: bool) MalType {
        return .{ .atom = if (b) .t else .f };
    }

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

    pub fn deinit(self: Self, allocator: Allocator) void {
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

    pub fn copy(self: Self, allocator: Allocator) Allocator.Error!MalType {
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

    pub fn equals(self: Self, other: *const Self) bool {
        // check if values are of the same type
        return @enumToInt(self) == @enumToInt(other.*) and switch (self) {
            .atom => |atom| @enumToInt(self.atom) == @enumToInt(other.atom) and switch (atom) {
                .number => |number| number == other.atom.number,
                .string => |string| std.mem.eql(u8, string, other.atom.string),
                .symbol => |symbol| std.mem.eql(u8, symbol, other.atom.symbol),
                .t, .f, .nil => true,
            },
            .list => |list| list.items.len == other.list.items.len and for (list.items) |item, i| {
                if (!item.equals(&other.list.items[i])) break false;
            } else true,
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
        pub const Parameters = std.ArrayList(MalType.Symbol);
        pub const Primitive = union(enum) {
            pub const Error = error{} || Allocator.Error || std.fs.File.WriteError;
            // unary primitives
            // op_val_out_val: fn (a: *const MalValue) MalValue,
            op_alloc_val_out_val: fn (allocator: Allocator, a: *const MalValue) Error!*MalValue,
            op_val_out_bool: fn (a: *const MalValue) bool,
            op_val_out_num: fn (a: *const MalValue) MalType.Number,
            // binary primitives
            op_num_num_out_bool: fn (a: MalType.Number, b: MalType.Number) bool,
            op_num_num_out_num: fn (a: MalType.Number, b: MalType.Number) MalType.Number,
            op_val_val_out_bool: fn (a: *const MalValue, b: *const MalValue) bool,
            // vargars primitives
            op_alloc_vargars_out_val: fn (allocator: Allocator, args: MalValue.List) Error!*MalValue,

            pub fn make(fn_ptr: anytype) MalValue {
                const type_info = @typeInfo(@TypeOf(fn_ptr));
                std.debug.assert(type_info == .Fn);
                const args = type_info.Fn.args;
                const return_type = type_info.Fn.return_type.?;
                return .{
                    .function = .{
                        .primitive = switch (args.len) {
                            1 => blk: {
                                const a_type = args[0].arg_type.?;
                                if (a_type == *const MalValue) {
                                    if (return_type == bool)
                                        break :blk .{ .op_val_out_bool = fn_ptr };
                                    if (return_type == MalType.Number)
                                        break :blk .{ .op_val_out_num = fn_ptr };
                                }
                            },
                            2 => blk: {
                                const a_type = args[0].arg_type.?;
                                const b_type = args[1].arg_type.?;
                                if (a_type == MalType.Number and b_type == MalType.Number) {
                                    if (return_type == bool)
                                        break :blk .{ .op_num_num_out_bool = fn_ptr };
                                    if (return_type == MalType.Number)
                                        break :blk .{ .op_num_num_out_num = fn_ptr };
                                }
                                if (a_type == *const MalValue and b_type == *const MalValue) {
                                    if (return_type == bool)
                                        break :blk .{ .op_val_val_out_bool = fn_ptr };
                                }
                                if (a_type == Allocator and b_type == *const MalValue) {
                                    // TODO: and return_type == Error!*MalValue
                                    break :blk .{ .op_alloc_val_out_val = fn_ptr };
                                }
                                if (a_type == Allocator and b_type == MalValue.List) {
                                    // TODO: and return_type == Error!*MalValue
                                    break :blk .{ .op_alloc_vargars_out_val = fn_ptr };
                                }
                            },
                            else => unreachable,
                        },
                    },
                };
            }
            pub fn eval(primitive: Primitive, allocator: Allocator, args: []const MalValue) !MalValue {
                // TODO: can probably be compile-time generated from function type info
                switch (primitive) {
                    .op_num_num_out_num => |op| {
                        if (args.len != 2) return error.EvalInvalidOperands;
                        const a = args[0].asNumber() catch return error.EvalInvalidOperand;
                        const b = args[1].asNumber() catch return error.EvalInvalidOperand;
                        return MalValue{ .mal_type = MalType.makeNumber(op(a, b)) };
                    },
                    .op_num_num_out_bool => |op| {
                        if (args.len != 2) return error.EvalInvalidOperands;
                        const a = args[0].asNumber() catch return error.EvalInvalidOperand;
                        const b = args[1].asNumber() catch return error.EvalInvalidOperand;
                        return MalValue{ .mal_type = MalType.makeBool(op(a, b)) };
                    },
                    .op_val_out_bool => |op| {
                        if (args.len != 1) return error.EvalInvalidOperands;
                        const a = &args[0];
                        return MalValue{ .mal_type = MalType.makeBool(op(a)) };
                    },
                    .op_val_out_num => |op| {
                        if (args.len != 1) return error.EvalInvalidOperands;
                        const a = &args[0];
                        return MalValue{ .mal_type = MalType.makeNumber(op(a)) };
                    },
                    .op_val_val_out_bool => |op| {
                        if (args.len != 2) return error.EvalInvalidOperands;
                        const a = &args[0];
                        const b = &args[1];
                        return MalValue{ .mal_type = MalType.makeBool(op(a, b)) };
                    },
                    .op_alloc_val_out_val => |op| {
                        if (args.len != 1) return error.EvalInvalidOperands;
                        const result_ptr = try op(allocator, &args[0]);
                        return result_ptr.*;
                    },
                    .op_alloc_vargars_out_val => |op| {
                        var args_list = try MalValue.List.initCapacity(allocator, args.len);
                        for (args) |arg| args_list.appendAssumeCapacity(arg);
                        const result_ptr = try op(allocator, args_list);
                        return result_ptr.*;
                    },
                }
            }
        };
        primitive: Primitive,
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

    pub fn initListAlloc(allocator: Allocator) MalValue {
        return MalValue{ .list = List.init(allocator) };
    }

    pub fn initListCapacity(allocator: Allocator, num: usize) !MalValue {
        return MalValue{ .list = try List.initCapacity(allocator, num) };
    }

    pub fn makeString(string: []const u8) MalValue {
        return .{ .mal_type = MalType.makeString(string) };
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

    pub fn copy(self: Self, allocator: Allocator) Allocator.Error!MalValue {
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
                        parameters_copy.appendAssumeCapacity(try allocator.dupe(u8, item));
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

    pub fn equals(self: Self, other: *const Self) bool {
        // TODO: check &self == other ?
        // check if values are of the same type
        return @enumToInt(self) == @enumToInt(other.*) and switch (self) {
            .mal_type => |mal_type| mal_type.equals(&other.mal_type),
            .list => |list| list.items.len == other.list.items.len and for (list.items) |item, i| {
                if (!item.equals(&other.list.items[i])) break false;
            } else true,
            .function => |function| @enumToInt(function) == @enumToInt(other.function) and switch (function) {
                .closure => |closure| blk: {
                    if (closure.env != other.function.closure.env) break :blk false;
                    if (!closure.body.equals(&other.function.closure.body)) break :blk false;
                    if (closure.parameters.items.len != other.function.closure.parameters.items.len) break :blk false;
                    for (closure.parameters.items) |item, i| {
                        if (!std.mem.eql(u8, item, other.function.closure.parameters.items[i])) break :blk false;
                    } else break :blk true;
                },
                .primitive => |primitive| @enumToInt(primitive) == @enumToInt(other.function.primitive) and
                    std.mem.eql(u8, std.mem.asBytes(&primitive), std.mem.asBytes(&other.function.primitive)),
            },
        };
    }

    pub fn isTruthy(self: Self) bool {
        return !(self == .mal_type and self.mal_type == .atom and (self.mal_type.atom == .f or self.mal_type.atom == .nil));
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

fn copyList(comptime T: type, _: std.ArrayList(T)) std.ArrayList(T) {}
