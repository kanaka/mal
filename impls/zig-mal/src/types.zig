const std = @import("std");
const Allocator = std.mem.Allocator;

const Env = @import("env.zig").Env;
const reader = @import("reader.zig");

pub const EvalError = error{
    EvalDefInvalidOperands,
    EvalDoInvalidOperands,
    EvalIfInvalidOperands,
    EvalLetInvalidOperands,
    EvalInvalidOperand,
    EvalInvalidOperands,
    EvalNotSymbolOrFn,
    EnvSymbolNotFound,
    EvalInvalidFnParamsList,
} || Allocator.Error || MalType.Primitive.Error;

pub const MalType = union(enum) {
    pub const List = std.ArrayList(*MalType);
    pub const Number = i32;

    const StrAlloc = struct { value: []const u8, allocator: Allocator };
    pub const Symbol = StrAlloc;
    pub const String = StrAlloc;

    pub const Parameters = std.ArrayList(Symbol);
    pub const Primitive = union(enum) {
        pub const Error = error{StreamTooLong} || Allocator.Error || std.fs.File.OpenError || std.fs.File.WriteError || std.os.ReadError || TypeError || reader.ReadError;
        // unary primitives
        op_alloc_val_out_val: fn (allocator: Allocator, a: *MalType) EvalError!*MalType,
        op_val_out_val: fn (a: *MalType) EvalError!*MalType,
        op_val_out_bool: fn (a: *MalType) bool,
        op_val_out_num: fn (a: *MalType) Number,
        // binary primitives
        op_num_num_out_bool: fn (a: Number, b: Number) bool,
        op_num_num_out_num: fn (a: Number, b: Number) Number,
        op_val_val_out_bool: fn (a: *MalType, b: *MalType) bool,
        op_val_val_out_val: fn (a: *MalType, b: *MalType) EvalError!*MalType,
        // varargs primitives
        op_alloc_varargs_out_val: fn (allocator: Allocator, args: List) EvalError!*MalType,

        pub fn make(fn_ptr: anytype) Primitive {
            const type_info = @typeInfo(@TypeOf(fn_ptr));
            std.debug.assert(type_info == .Fn);
            const args = type_info.Fn.args;
            const return_type = type_info.Fn.return_type.?;
            return switch (args.len) {
                1 => blk: {
                    const a_type = args[0].arg_type.?;
                    if (a_type == *MalType) {
                        if (return_type == bool)
                            break :blk .{ .op_val_out_bool = fn_ptr };
                        if (return_type == Number)
                            break :blk .{ .op_val_out_num = fn_ptr };
                        break :blk .{ .op_val_out_val = fn_ptr };
                    }
                },
                2 => blk: {
                    const a_type = args[0].arg_type.?;
                    const b_type = args[1].arg_type.?;
                    if (a_type == Number and b_type == Number) {
                        if (return_type == bool)
                            break :blk .{ .op_num_num_out_bool = fn_ptr };
                        if (return_type == Number)
                            break :blk .{ .op_num_num_out_num = fn_ptr };
                    }
                    if (a_type == *MalType and b_type == *MalType) {
                        if (return_type == bool)
                            break :blk .{ .op_val_val_out_bool = fn_ptr };

                        break :blk .{ .op_val_val_out_val = fn_ptr };
                    }
                    if (a_type == Allocator and b_type == *MalType) {
                        // TODO: and return_type == Error!*MalType
                        break :blk .{ .op_alloc_val_out_val = fn_ptr };
                    }
                    if (a_type == Allocator and b_type == List) {
                        // TODO: and return_type == Error!*MalType
                        break :blk .{ .op_alloc_varargs_out_val = fn_ptr };
                    }
                },
                else => unreachable,
            };
        }
        pub fn apply(primitive: Primitive, allocator: Allocator, args: []*MalType) !*MalType {
            // TODO: can probably be compile-time generated from function type info
            switch (primitive) {
                .op_num_num_out_num => |op| {
                    if (args.len != 2) return error.EvalInvalidOperands;
                    const a = args[0].asNumber() catch return error.EvalInvalidOperand;
                    const b = args[1].asNumber() catch return error.EvalInvalidOperand;
                    return makeNumber(allocator, op(a, b));
                },
                .op_num_num_out_bool => |op| {
                    if (args.len != 2) return error.EvalInvalidOperands;
                    const a = args[0].asNumber() catch return error.EvalInvalidOperand;
                    const b = args[1].asNumber() catch return error.EvalInvalidOperand;
                    return makeBool(allocator, op(a, b));
                },
                .op_val_out_bool => |op| {
                    if (args.len != 1) return error.EvalInvalidOperands;
                    return makeBool(allocator, op(args[0]));
                },
                .op_val_out_num => |op| {
                    if (args.len != 1) return error.EvalInvalidOperands;
                    return makeNumber(allocator, op(args[0]));
                },
                .op_val_val_out_bool => |op| {
                    if (args.len != 2) return error.EvalInvalidOperands;
                    return makeBool(allocator, op(args[0], args[1]));
                },
                .op_val_out_val => |op| {
                    if (args.len != 1) return error.EvalInvalidOperands;
                    return op(args[0]);
                },
                .op_val_val_out_val => |op| {
                    if (args.len != 2) return error.EvalInvalidOperands;
                    return op(args[0], args[1]);
                },
                .op_alloc_val_out_val => |op| {
                    if (args.len != 1) return error.EvalInvalidOperands;
                    return op(allocator, args[0]);
                },
                .op_alloc_varargs_out_val => |op| {
                    var args_list = try List.initCapacity(allocator, args.len);
                    for (args) |arg| args_list.appendAssumeCapacity(arg);
                    return op(allocator, args_list);
                },
            }
        }
    };

    pub const Closure = struct {
        parameters: Parameters,
        body: *MalType,
        env: *Env,
        eval: fn (allocator: Allocator, ast: *MalType, env: *Env) EvalError!*MalType,

        pub fn apply(closure: Closure, allocator: Allocator, args: []*MalType) !*MalType {
            const parameters = closure.parameters.items;
            if (parameters.len != args.len) {
                return error.EvalInvalidOperands;
            }
            // convert from a list of MalType.Symbol to a list of valid symbol keys to use in environment init
            var binds = try std.ArrayList([]const u8).initCapacity(allocator, parameters.len);
            for (parameters) |parameter| {
                binds.appendAssumeCapacity(parameter.value);
            }
            var fn_env_ptr = try closure.env.initChildBindExprs(binds.items, args);
            return closure.eval(allocator, closure.body, fn_env_ptr);
        }
    };

    pub const Atom = *MalType;

    pub const TypeError = error{
        NotAtom,
        NotFunction,
        NotList,
        NotNumber,
        NotSymbol,
        NotString,
    };

    // atoms
    t,
    f,
    nil,
    number: Number,
    string: String,
    symbol: Symbol,

    // TODO: keywords
    // TODO: vectors
    // TODO: hash-maps

    list: List,

    // functions
    primitive: Primitive,
    closure: Closure,

    atom: Atom,

    pub fn make(allocator: Allocator, value: MalType) !*MalType {
        var result_ptr = try allocator.create(MalType);
        result_ptr.* = value;
        return result_ptr;
    }

    pub fn makeBool(allocator: Allocator, b: bool) !*MalType {
        return make(allocator, if (b) .t else .f);
    }

    pub fn makeNumber(allocator: Allocator, num: Number) !*MalType {
        return make(allocator, .{ .number = num });
    }

    pub fn makeString(allocator: Allocator, string: []const u8) !*MalType {
        return make(allocator, .{ .string = .{ .value = string, .allocator = allocator } });
    }

    pub fn makeSymbol(allocator: Allocator, symbol: []const u8) !*MalType {
        return make(allocator, .{ .symbol = .{ .value = symbol, .allocator = allocator } });
    }

    pub fn makeAtom(allocator: Allocator, value: *MalType) !*MalType {
        return make(allocator, .{ .atom = value });
    }

    pub fn makeList(allocator: Allocator, list: List) !*MalType {
        return make(allocator, .{ .list = list });
    }

    pub fn makeListEmpty(allocator: Allocator) !*MalType {
        return make(allocator, .{ .list = List.init(allocator) });
    }

    pub fn makeListCapacity(allocator: Allocator, num: usize) !*MalType {
        return make(allocator, .{ .list = try List.initCapacity(allocator, num) });
    }

    pub fn makeListFromSlice(allocator: Allocator, slice: []*MalType) !*MalType {
        var list = try List.initCapacity(allocator, slice.len);
        for (slice) |item| {
            list.appendAssumeCapacity(item);
        }
        return make(allocator, .{ .list = list });
    }

    pub fn makeNil(allocator: Allocator) !*MalType {
        return make(allocator, .nil);
    }

    pub fn makePrimitive(allocator: Allocator, primitive: anytype) !*MalType {
        return make(allocator, .{ .primitive = Primitive.make(primitive) });
    }

    pub fn makeClosure(allocator: Allocator, closure: Closure) !*MalType {
        return make(allocator, .{ .closure = closure });
    }

    const Self = @This();

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            .list => |list| {
                for (list.items) |item| {
                    item.deinit();
                }
                list.deinit();
            },
            .string, .symbol => |str_alloc| str_alloc.allocator.free(str_alloc.value),
            .closure => |closure| {
                for (closure.parameters.items) |parameter| {
                    parameter.allocator.free(parameter.value);
                }
                closure.parameters.deinit();
                closure.body.deinit();
                // closure.env.deinit();
            },
            else => {},
        }
    }

    pub fn copy(self: Self, allocator: Allocator) Allocator.Error!*MalType {
        return switch (self) {
            .list => |list| blk: {
                var list_copy = try List.initCapacity(allocator, list.items.len);
                for (list.items) |item| {
                    list_copy.appendAssumeCapacity(try item.copy(allocator));
                }
                break :blk makeList(allocator, list_copy);
            },
            .string => |string| makeString(allocator, try allocator.dupe(u8, string.value)),
            .symbol => |symbol| makeSymbol(allocator, try allocator.dupe(u8, symbol.value)),
            .closure => |closure| blk: {
                var parameters_copy = try Parameters.initCapacity(allocator, closure.parameters.items.len);
                for (closure.parameters.items) |item| {
                    parameters_copy.appendAssumeCapacity(.{ .value = try allocator.dupe(u8, item.value), .allocator = allocator });
                }
                break :blk makeClosure(allocator, .{
                    .parameters = parameters_copy,
                    .body = try closure.body.copy(allocator),
                    .env = closure.env,
                });
            },
            // TODO: check this
            .atom => |atom| makeAtom(allocator, atom),
            else => make(allocator, self),
        };
    }

    pub fn equals(self: Self, other: *const Self) bool {
        // check if values are of the same type
        return @enumToInt(self) == @enumToInt(other.*) and switch (self) {
            .number => |number| number == other.number,
            .string => |string| std.mem.eql(u8, string.value, other.string.value),
            .symbol => |symbol| std.mem.eql(u8, symbol.value, other.symbol.value),
            .t, .f, .nil => true,
            .list => |list| list.items.len == other.list.items.len and for (list.items) |item, i| {
                if (!item.equals(other.list.items[i])) break false;
            } else true,
            .closure => |closure| blk: {
                if (closure.env != other.closure.env) break :blk false;
                if (!closure.body.equals(other.closure.body)) break :blk false;
                if (closure.parameters.items.len != other.closure.parameters.items.len) break :blk false;
                for (closure.parameters.items) |item, i| {
                    if (!std.mem.eql(u8, item.value, other.closure.parameters.items[i].value)) break :blk false;
                } else break :blk true;
            },
            .primitive => |primitive| @enumToInt(primitive) == @enumToInt(other.primitive) and
                std.mem.eql(u8, std.mem.asBytes(&primitive), std.mem.asBytes(&other.primitive)),
            .atom => &self == other,
        };
    }

    pub fn isTruthy(self: Self) bool {
        return !(self == .f or self == .nil);
    }

    pub fn asList(self: Self) !List {
        return switch (self) {
            .list => |list| list,
            else => error.NotList,
        };
    }

    pub fn asNumber(self: Self) !Number {
        return switch (self) {
            .number => |number| number,
            else => error.NotNumber,
        };
    }

    pub fn asString(self: Self) !String {
        return switch (self) {
            .string => |string| string,
            else => error.NotString,
        };
    }

    pub fn asSymbol(self: Self) !Symbol {
        return switch (self) {
            .symbol => |symbol| symbol,
            else => error.NotSymbol,
        };
    }

    pub fn asAtom(self: Self) !Atom {
        return switch (self) {
            .atom => |atom| atom,
            else => error.NotAtom,
        };
    }

    pub fn apply(self: Self, allocator: Allocator, args: []*MalType) !*MalType {
        return switch (self) {
            .primitive => |primitive| primitive.apply(allocator, args),
            .closure => |closure| closure.apply(allocator, args),
            else => error.NotFunction,
        };
    }
};
