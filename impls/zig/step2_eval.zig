const std = @import("std");
const warn = @import("std").debug.warn;

const reader = @import("reader.zig");
const pcre = reader.pcre;
const printer = @import("printer.zig");
const getline = @import("readline.zig").getline;
const string_copy = @import("utils.zig").string_copy;
const string_concat = @import("utils.zig").string_concat;
const apply_function = @import("types.zig").apply_function;
const linked_list = @import("linked_list.zig");
const hash_map = @import("hmap.zig");

const Allocator = @import("std").heap.c_allocator;

const MalType = @import("types.zig").MalType;
const MalData = @import("types.zig").MalData;
const MalError = @import("error.zig").MalError;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const MalHashMap = hash_map.MalHashMap;

var repl_environment: *MalHashMap = undefined;

fn READ(a: [] u8) MalError!?*MalType {
    var read = try reader.read_str(a);
    var optional_mal = reader.read_form(&read);
    return optional_mal;
}

fn EVAL(mal: *MalType) MalError!*MalType {
    switch(mal.data) {
        .List => |ll| {
            if(ll.len == 0) {
                return mal;
            }
            var new_list = try eval_ast(mal);
            return apply_function(Allocator, (try new_list.sequence_linked_list()).*);
        },
        else => {
            return eval_ast(mal);
        },
    }
}

fn PRINT(optional_mal: ?*MalType) MalError![] u8 {
    return printer.print_str(optional_mal);
}

fn rep(input: [] u8) MalError!?[] u8 {
    var read_input = (try READ(input)) orelse return null;
    var eval_input = try EVAL(read_input);
    var print_input = try PRINT(eval_input);
    eval_input.delete(Allocator);
    return print_input;
}

fn lookup(symbol: []const u8, do_warn: bool) MalError!*MalType {
    var optional_mal = repl_environment.getValue(symbol);
    if(optional_mal) |mal| {
        return mal.copy(Allocator);
    }
    if(do_warn) {
        const s1 = string_concat(Allocator, "'", symbol) catch return MalError.SystemError;
        const s2 = string_concat(Allocator, s1, "' not found") catch return MalError.SystemError;
        defer Allocator.free(s1);
        defer Allocator.free(s2);
        warn("'{}' not found.\n", symbol);
    }
    return MalError.KeyError;
}

fn eval_ast(mal: *MalType) MalError!*MalType {
    switch(mal.data) {
        .Generic => |symbol| {
            defer mal.delete(Allocator);
            return lookup(symbol, true);
        },
        .List => |*ll| {
            var new_ll = MalLinkedList.init(Allocator);
            var iterator = ll.iterator();
            while(iterator.next()) |next_mal| {
                const new_mal = try EVAL(next_mal);
                try linked_list.append_mal(Allocator, &new_ll, new_mal);
            }
            linked_list.destroy(Allocator, ll, true);
            mal.shallow_destroy(Allocator);
            const ret_mal = MalType.new_list(Allocator, new_ll);
            return ret_mal;
        },
        .Vector => |*ll| {
            var new_ll = MalLinkedList.init(Allocator);
            var iterator = ll.iterator();
            while(iterator.next()) |next_mal| {
                const new_mal = try EVAL(next_mal);
                try linked_list.append_mal(Allocator, &new_ll, new_mal);
            }
            linked_list.destroy(Allocator, ll, true);
            mal.shallow_destroy(Allocator);
            const ret_mal = MalType.new_vector(Allocator, new_ll);
            return ret_mal;
        },
        .HashMap => |hmap| {
            var new_hashmap = try MalType.new_hashmap(Allocator);
            var iterator = hmap.iterator();
            var optional_pair = iterator.next();
            while(true) {
                const pair = optional_pair orelse break;
                const key = pair.key;
                const value = pair.value;
                const evaled_value = try EVAL(value);
                try new_hashmap.hashmap_insert(key, evaled_value);
                optional_pair = iterator.next();
            }
            hash_map.destroy(Allocator, hmap, true);
            mal.shallow_destroy(Allocator);
            return new_hashmap;
        },
        else => {
            return mal;
        }
    }
}

const safeAdd = @import("std").math.add;
const safeSub = @import("std").math.sub;
const safeMul = @import("std").math.mul;
const safeDivFloor = @import("std").math.divFloor;

fn int_plus(a1: *MalType, a2: *MalType) MalError!*MalType {
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = safeAdd(i64, x, y) catch return MalError.Overflow;
    return MalType.new_int(Allocator, res);
}

fn int_minus(a1: *MalType, a2: *MalType) MalError!*MalType {
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = safeSub(i64, x, y) catch return MalError.Overflow;
    return MalType.new_int(Allocator, res);
}

fn int_mult(a1: *MalType, a2: *MalType) MalError!*MalType {
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = safeMul(i64, x, y) catch return MalError.Overflow;
    return MalType.new_int(Allocator, res);
}

fn int_div(a1: *MalType, a2: *MalType) MalError!*MalType {
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = safeDivFloor(i64, x, y) catch |err| switch(err) {
        error.DivisionByZero => return MalError.DivisionByZero,
        else => return MalError.Overflow,
    };
    return MalType.new_int(Allocator, res);
}

fn make_environment() MalError!void {
    repl_environment = Allocator.create(MalHashMap) catch return MalError.SystemError;
    repl_environment.* = MalHashMap.init(Allocator);

    const plus_mal = try MalType.new_nil(Allocator);
    plus_mal.data = MalData{.Fn2 = &int_plus};
    _ = repl_environment.put("+", plus_mal) catch return MalError.SystemError;
    const minus_mal = try MalType.new_nil(Allocator);
    minus_mal.data = MalData{.Fn2 = &int_minus};
    _ = repl_environment.put("-", minus_mal) catch return MalError.SystemError;
    const mult_mal = try MalType.new_nil(Allocator);
    mult_mal.data = MalData{.Fn2 = &int_mult};
    _ = repl_environment.put("*", mult_mal) catch return MalError.SystemError;
    const div_mal = try MalType.new_nil(Allocator);
    div_mal.data = MalData{.Fn2 = &int_div};
    _ = repl_environment.put("/", div_mal) catch return MalError.SystemError;
}

pub fn main() !void {
    const stdout_file = try std.io.getStdOut();
    try make_environment();
    while(true) {
        var line = (try getline(Allocator)) orelse break;
        var optional_output = rep(line) catch |err| {
            if(err == MalError.KeyError) {
                continue;
            } else {
                return err;
            }
        };
        if(optional_output) |output| {
            try stdout_file.write(output);
            Allocator.free(output);
            try stdout_file.write("\n");
        }
    }
}
