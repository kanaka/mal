const std = @import("std");
const Allocator = std.mem.Allocator;

const printer = @import("./printer.zig");
const types = @import("./types.zig");
const MalType = types.MalType;
const MalValue = types.MalValue;
const Number = MalType.Number;
const Primitive = MalValue.Function.Primitive;
const Error = Primitive.Error;

pub fn add(a: Number, b: Number) Number {
    return a + b;
}

pub fn subtract(a: Number, b: Number) Number {
    return a - b;
}

pub fn multiply(a: Number, b: Number) Number {
    return a * b;
}

pub fn divide(a: Number, b: Number) Number {
    // TODO: use std.math.divFloor/divTrunc for runtime errors instead of
    // undefined behavior when dividing by zero
    return @divFloor(a, b);
}

pub fn lessThan(a: Number, b: Number) bool {
    return a < b;
}

pub fn lessOrEqual(a: Number, b: Number) bool {
    return a <= b;
}

pub fn greaterThan(a: Number, b: Number) bool {
    return a > b;
}

pub fn greaterOrEqual(a: Number, b: Number) bool {
    return a >= b;
}

pub fn list(allocator: *Allocator, params: MalValue.List) Error!*MalValue {
    var result_ptr = try allocator.create(MalValue);
    result_ptr.* = MalValue{ .list = params };
    return result_ptr;
}

pub fn is_list(param: *const MalValue) bool {
    return param.* == .list;
}

pub fn is_empty(param: *const MalValue) bool {
    return count(param) == 0;
}

pub fn count(param: *const MalValue) Number {
    // TODO: error if not list
    return if (is_list(param)) @intCast(Number, param.list.items.len) else -1;
}

pub fn eql(a: *const MalValue, b: *const MalValue) bool {
    return a.equals(b);
}

pub fn prn(allocator: *Allocator, param: *const MalValue) Error!*MalValue {
    const str = try printer.pr_str(allocator, param);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{str});
    // TODO: this shouldn't need to allocate
    var result_ptr = try allocator.create(MalValue);
    result_ptr.* = MalValue{ .mal_type = .{ .atom = .nil } };
    return result_ptr;
}

pub const ns = .{
    .@"+" = Primitive.make(add),
    .@"-" = Primitive.make(subtract),
    .@"*" = Primitive.make(multiply),
    .@"/" = Primitive.make(divide),
    .@"<" = Primitive.make(lessThan),
    .@"<=" = Primitive.make(lessOrEqual),
    .@">" = Primitive.make(greaterThan),
    .@">=" = Primitive.make(greaterOrEqual),
    .@"=" = Primitive.make(eql),
    .@"list" = Primitive.make(list),
    .@"list?" = Primitive.make(is_list),
    .@"empty?" = Primitive.make(is_empty),
    .@"count" = Primitive.make(count),
    .@"prn" = Primitive.make(prn),
};

pub const ns_map = std.ComptimeStringMap(MalValue, .{
    .{ "+", Primitive.make(add) },
    .{ "-", Primitive.make(subtract) },
    .{ "*", Primitive.make(multiply) },
    .{ "/", Primitive.make(divide) },
    .{ "<", Primitive.make(lessThan) },
    .{ "<=", Primitive.make(lessOrEqual) },
    .{ ">", Primitive.make(greaterThan) },
    .{ ">=", Primitive.make(greaterOrEqual) },
});

pub const ns_fn_ptrs = .{
    .add = add,
    .subtract = subtract,
    .multiply = multiply,
    .divide = divide,
    .prn = prn,
    .list = list,
    .is_list = is_list,
    .is_empty = is_empty,
    .eql = eql,
    .lessThan = lessThan,
    .greaterThan = greaterThan,
    .lessOrEqual = lessOrEqual,
    .greaterOrEqual = greaterOrEqual,
};
