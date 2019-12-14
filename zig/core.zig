const std = @import("std");
const warn = @import("std").debug.warn;

const AllocatorType = @import("std").mem.Allocator;
var Allocator: *AllocatorType = undefined;

pub fn set_allocator(alloc: *AllocatorType) void {
    Allocator = alloc;
}

const Env = @import("env.zig").Env;
const MalData = @import("types.zig").MalData;
const MalType = @import("types.zig").MalType;
const MalTypeValue = @import("types.zig").MalTypeValue;
const printer = @import("printer.zig");
const reader = @import("reader.zig");
const getline_prompt = @import("readline.zig").getline_prompt;
const string_eql = @import("utils.zig").string_eql;
const string_copy = @import("utils.zig").string_copy;

const MalError = @import("error.zig").MalError;

const hmap = @import("hmap.zig");

const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const MalHashMap = @import("hmap.zig").MalHashMap;
const linked_list = @import("linked_list.zig");
const apply_function = @import("types.zig").apply_function;

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

fn int_lt(a1: *MalType, a2: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, (try a1.as_int()) < (try a2.as_int()));
}

fn int_leq(a1: *MalType, a2: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, (try a1.as_int()) <= (try a2.as_int()));
}

fn int_gt(a1: *MalType, a2: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, (try a1.as_int()) > (try a2.as_int()));
}

fn int_geq(a1: *MalType, a2: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, (try a1.as_int()) >= (try a2.as_int()));
}

fn _linked_list_equality(l1: MalLinkedList, l2: MalLinkedList) MalError!bool {
    if(l1.count() != l2.count()) {
        return false;
    }
    var it1 = l1.iterator();
    var it2 = l2.iterator();
    while(true) {
        const m1 = it1.next() orelse return (it2.next() == null);
        const m2 = it2.next() orelse return false;
        const el_cmp = try equality(m1, m2);
        if(MalTypeValue(el_cmp.data) == MalTypeValue.False) {
            el_cmp.delete(Allocator);
            return false;
        }
        el_cmp.delete(Allocator);
    }
    return true;
}

fn _hashmap_equality(h1: MalHashMap, h2: MalHashMap) MalError!bool {
    if(h1.count() != h2.count()) {
        return false;
    }

    var iterator = h1.iterator();
    var optional_pair = iterator.next();
    while(optional_pair) |pair| {
        const optional_val = h2.getValue(pair.key);
        if(optional_val) |val| {
            const el_cmp = try equality(pair.value, val);
            if(MalTypeValue(el_cmp.data) == MalTypeValue.False) {
                el_cmp.delete(Allocator);
                return false;
            }
            el_cmp.delete(Allocator);
        }
        else {
            return false;
        }
        optional_pair = iterator.next();
    }
    return true;
}

// TODO: make _equality -> bool
fn equality(a1: *MalType, a2: *MalType) MalError!*MalType {
    const a1_is_sequential = (MalTypeValue(a1.data) == MalTypeValue.List) or
        (MalTypeValue(a1.data) == MalTypeValue.Vector);
    const a2_is_sequential = (MalTypeValue(a2.data) == MalTypeValue.List) or
        (MalTypeValue(a2.data) == MalTypeValue.Vector);

    if(a1_is_sequential and a2_is_sequential) {
        const l1 = (try a1.sequence_linked_list()).*;
        const l2 = (try a2.sequence_linked_list()).*;
        return MalType.new_bool(Allocator, try _linked_list_equality(l1, l2));
    }
    
    if(MalTypeValue(a1.data) != MalTypeValue(a2.data)) {
        return MalType.new_bool(Allocator, false);
    }
    
    switch(a1.data) {
        .True, .False, .Nil => {            
            return MalType.new_bool(Allocator, true);
        },
        .Int => |v1| {
            return MalType.new_bool(Allocator, v1 == a2.data.Int);
        },
        .List => |l1| {
            const l2 = a2.data.List;
            return MalType.new_bool(Allocator, try _linked_list_equality(l1, l2));
        },
        .Vector => |v1| {
            const v2 = a2.data.Vector;
            return MalType.new_bool(Allocator, try _linked_list_equality(v1, v2));
        },
        .String => |s1| {
            const s2 = a2.data.String;
            return MalType.new_bool(Allocator, string_eql(s1, s2));
        },
        .Generic => |v1| {
            const v2 = a2.data.Generic;
            return MalType.new_bool(Allocator, string_eql(v1, v2));
        },
        .Keyword => |k1| {
            const k2 = a2.data.Keyword;
            return MalType.new_bool(Allocator, string_eql(k1, k2));
        },
        .HashMap => |h1| {
            const h2 = a2.data.HashMap;
            return MalType.new_bool(Allocator, try _hashmap_equality(h1,h2));
        },
    // TODO: implement more types
        else => return MalType.new_bool(Allocator, false),
    }
}

fn list(args: MalLinkedList) MalError!*MalType {
    var new_mal = try MalType.new_list_empty(Allocator);
    new_mal.data = MalData{.List = try linked_list.deepcopy(Allocator, args)};
    return new_mal;
}

fn vector(args: MalLinkedList) MalError!*MalType {
    var new_mal = try MalType.new_list_empty(Allocator);
    new_mal.data = MalData{.Vector = try linked_list.deepcopy(Allocator, args)};
    return new_mal;
}

fn map(args: MalLinkedList) MalError!*MalType {
    if(args.count() < 2) return MalError.ArgError;
    const func_mal = args.at(0);
    var args_mal = args.at(1);
    var new_ll = MalLinkedList.init(Allocator);
    var to_map_ll = try args_mal.sequence_linked_list();
 
    var iterator = to_map_ll.iterator();
    while(iterator.next()) |mal| {
        var args_ll = MalLinkedList.init(Allocator);
        // TODO: can be more efficient than this
        try linked_list.append_mal(Allocator, &args_ll, try func_mal.copy(Allocator));
        try linked_list.append_mal(Allocator, &args_ll, try mal.copy(Allocator));
        const new_mal = try apply_function(Allocator, args_ll);
        linked_list.destroy(Allocator, &args_ll, false);
        try linked_list.append_mal(Allocator, &new_ll, new_mal);
    }
    const new_list = try MalType.new_nil(Allocator);
    new_list.data = MalData{.List = new_ll};
    return new_list;
}

fn is_list(a1: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, MalTypeValue(a1.data) == MalTypeValue.List);
}

fn is_vector(a1: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, MalTypeValue(a1.data) == MalTypeValue.Vector);
}

pub fn is_string(a1: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, MalTypeValue(a1.data) == MalTypeValue.String);
}

pub fn is_number(a1: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, MalTypeValue(a1.data) == MalTypeValue.Int);
}

pub fn is_fn(a1: *MalType) MalError!*MalType {
    const is_function = switch(a1.data) {
        .Fn0 => true,
        .Fn1 => true,
        .Fn2 => true,
        .Fn3 => true,
        .Fn4 => true,
        .FVar => true,
        .Func => |func_data| !func_data.is_macro,
        else => false,
    };
    return MalType.new_bool(Allocator, is_function);
}

pub fn is_macro(a1: *MalType) MalError!*MalType {
    const is_func_and_macro = switch(a1.data) {
        .Func => |data| data.is_macro,
        else => false,
    };
    return MalType.new_bool(Allocator, is_func_and_macro);
}

fn empty(a1: *MalType) MalError!*MalType {
    return switch(a1.data) {
        .List => |l| MalType.new_bool(Allocator, l.len == 0),
        .Vector => |v| MalType.new_bool(Allocator, v.len == 0),
        else => MalType.new_bool(Allocator, false),
    };
}

fn prn(args: MalLinkedList) MalError!*MalType {
    const s = try printer.print_mal_to_string(args, true, true);
    const stdout_file = std.io.getStdOut() catch return MalError.SystemError;
    stdout_file.write(s) catch return MalError.SystemError;
    stdout_file.write("\n") catch return MalError.SystemError;
    Allocator.free(s);
    const mal = try MalType.new_nil(Allocator);
    return mal;
}

fn println(args: MalLinkedList) MalError!*MalType {
    const s = try printer.print_mal_to_string(args, false, true);
    const stdout_file = std.io.getStdOut() catch return MalError.SystemError;
    stdout_file.write(s) catch return MalError.SystemError;
    stdout_file.write("\n") catch return MalError.SystemError;
    Allocator.free(s);
    const mal = try MalType.new_nil(Allocator);
    return mal;
}

fn str(args: MalLinkedList) MalError!*MalType {
    if(args.count() == 0) {
        const s: []u8 = "";
        return MalType.new_string(Allocator, s);
    }
    const s = try printer.print_mal_to_string(args, false, false);
    return MalType.new_string(Allocator, s);
}

fn pr_str(args: MalLinkedList) MalError!*MalType {
    if(args.count() == 0) {
        const s: []u8 = "";
        return MalType.new_string(Allocator, s);
    }
    const s = try printer.print_mal_to_string(args, true, true);
    return MalType.new_string(Allocator, s);
}

fn slurp(a1: *MalType) MalError!*MalType {
    switch(a1.data) {
        .String => |path| {
            const file_contents = std.io.readFileAlloc(Allocator, path)
                catch |err| return MalError.SystemError; // TODO: change this error
            defer Allocator.free(file_contents);
            return MalType.new_string(Allocator, file_contents);
        },
        else => {
            return MalError.TypeError;
        },
    }
    return unreachable;
}

fn atom(a1: *MalType) MalError!*MalType {
    return MalType.new_atom(Allocator, a1);
}

fn is_atom(a1: *MalType) MalError!*MalType {
    return MalType.new_bool(Allocator, MalTypeValue(a1.data) == MalTypeValue.Atom);
}

fn deref(a1: *MalType) MalError!*MalType {
    return switch(a1.data) {
        .Atom => |atom_val| atom_val.*.copy(Allocator),
        else => MalError.TypeError,
    };
}

fn atom_reset(a1: *MalType, a2: *MalType) MalError!*MalType {
    switch(a1.data) {
        .Atom => |*atom_val| {
            var new_target = try a2.copy(Allocator);
            atom_val.*.*.delete(Allocator);
            atom_val.*.* = new_target;
            return new_target.copy(Allocator);
        },
        else => return MalError.TypeError,
    }
}

fn atom_swap(args: MalLinkedList) MalError!*MalType {
    const args_arr = args.toSlice();
    const n = args.len;
    if(n < 2) return MalError.ArgError;
    var new_args = MalLinkedList.init(Allocator);
    defer linked_list.destroy(Allocator, &new_args, false);
    try linked_list.append_mal(Allocator, &new_args, try args_arr[1].copy(Allocator));
    try linked_list.append_mal(Allocator, &new_args, try deref(args_arr[0]));
    var i: usize = 2;
    while(i < n) {
        try linked_list.append_mal(Allocator, &new_args, try args_arr[i].copy(Allocator));
        i += 1;
    }
    const return_mal = try apply_function(Allocator, new_args);
    const new_mal = atom_reset(args_arr[0], return_mal);
    return_mal.delete(Allocator);
    return new_mal;
}

pub fn cons(a1: *const MalType, a2: *const MalType) MalError!*MalType {
    // TODO: do we need this for vectors?
    const old_ll = try a2.const_sequence_linked_list();
    var new_ll = try linked_list.deepcopy(Allocator, old_ll);
    var new_list = try MalType.new_nil(Allocator);
    new_list.data = MalData{.List = new_ll};
    errdefer new_list.delete(Allocator);
    var new_mal = try a1.copy(Allocator);
    errdefer new_mal.delete(Allocator);
    try linked_list.prepend_mal(Allocator, &new_list.data.List, new_mal);
    return new_list;
}

pub fn concat(args: MalLinkedList) MalError!*MalType {
    // First we make a new array with shallow copies
    var new_ll = MalLinkedList.init(Allocator);
    errdefer linked_list.destroy(Allocator, &new_ll, false);
    var iterator = args.iterator();
    while(iterator.next()) |mal| {
        const mal_seq = try mal.sequence_linked_list();
        new_ll.appendSlice(mal_seq.toSlice()) catch return MalError.SystemError;
    }

    // Now we turn the shallow copies into deep copies
    const new_arr = new_ll.toSlice();
    var i: usize = 0;
    while(i < new_arr.len) {
        new_arr[i] = try new_arr[i].copy(Allocator);
        i += 1;
    }

    // Wrap the list in a MalType, return
    var new_mal = try MalType.new_nil(Allocator);
    new_mal.data = MalData{.List = new_ll};
    return new_mal;
}

pub fn rest(a1: *const MalType) MalError!*MalType {
    var old_list = switch(a1.data) {
        .List => |l| l,
        .Vector => |v| v,
        .Nil => return MalType.new_list_empty(Allocator),
        else => return MalError.TypeError,
    };
    var new_list = try linked_list.deepcopy(Allocator, old_list);
    errdefer linked_list.destroy(Allocator, &new_list, false);

    if(new_list.count() > 0) {
        const mal = try linked_list.pop_first(Allocator, &new_list);
        mal.delete(Allocator);
    }
    var new_mal = try MalType.new_nil(Allocator);
    new_mal.data = MalData{.List = new_list};
    return new_mal;
}

pub fn _nth(mal_list: *const MalType, pos: i64) MalError!*MalType {
    // TODO: vectors?
    const l = try mal_list.const_sequence_linked_list();
    if(pos < 0 or pos >= @intCast(i64,l.count())) {
        return MalError.OutOfBounds;
    }
    return l.at(@intCast(usize,pos));
}

pub fn nth(a1: *const MalType, a2: *const MalType) MalError!*MalType {
    return switch(a2.data) {
        .Int => |pos| (try _nth(a1, pos)).copy(Allocator),
        else => MalError.TypeError,
    };
}

pub fn first(a1: *const MalType) MalError!*MalType {
    var l = switch(a1.data) {
        .List => |l| l,
        .Vector => |v| v,
        .Nil => return MalType.new_nil(Allocator),
        else => return MalError.TypeError,
    };
    if(l.count() == 0) return MalType.new_nil(Allocator);
    return l.at(0).copy(Allocator);
}

fn check_type(mal: *const MalType, value_type: MalTypeValue) MalError!*MalType {
    // TODO: use this everywhere
    // TODO: do this more generically
    return MalType.new_bool(Allocator, MalTypeValue(mal.data) == value_type);
}

pub fn is_nil(a1: *const MalType) MalError!*MalType {
    return check_type(a1, MalTypeValue.Nil);
}

pub fn is_true(a1: *const MalType) MalError!*MalType {
    return check_type(a1, MalTypeValue.True);
}

pub fn is_false(a1: *const MalType) MalError!*MalType {
    return check_type(a1, MalTypeValue.False);
}

pub fn is_symbol(a1: *const MalType) MalError!*MalType {
    return check_type(a1, MalTypeValue.Generic);
}

pub fn is_keyword(a1: *const MalType) MalError!*MalType {
    return check_type(a1, MalTypeValue.Keyword);
}

pub fn is_map(a1: *const MalType) MalError!*MalType {
    return check_type(a1, MalTypeValue.HashMap);
}

pub fn is_sequential(a1: *const MalType) MalError!*MalType {
    const res = (MalTypeValue(a1.data) == MalTypeValue.Vector) or
        (MalTypeValue(a1.data) == MalTypeValue.List);
    return MalType.new_bool(Allocator, res);
}

pub fn symbol(a1: *const MalType) MalError!*MalType {
    const string = switch(a1.data) {
        .String => |s| s,
        else => return MalError.TypeError,
    };
    return MalType.new_generic(Allocator, string);
}

pub fn hash_map(args: MalLinkedList) MalError!*MalType {
    const new_mal = try MalType.new_hashmap(Allocator);
    const args_arr = args.toSlice();
    const n = args_arr.len;
    if((n%2) != 0) return MalError.ArgError;
    var i: usize = 0;

    while(2*i+1 < n) {
        const this_key = switch(args_arr[2*i].data) {
            .String => |s| s,
            .Keyword => |kwd| kwd,
            else => return MalError.ArgError,
        };
        const this_key_cpy = string_copy(Allocator, this_key) catch return MalError.SystemError;
        const this_val_cpy = try args_arr[2*i+1].copy(Allocator);
        try new_mal.hashmap_insert(this_key_cpy, this_val_cpy);
        i += 1;
    }
    return new_mal;
}

pub fn hash_map_assoc(args: MalLinkedList) MalError!*MalType {
    const args_arr = args.toSlice();
    if(args_arr.len < 1) return MalError.ArgError;
    const new_mal = try MalType.new_nil(Allocator);
    errdefer new_mal.delete(Allocator);
    const base_hmap = switch(args_arr[0].data) {
        .HashMap => |hm| hm,
        else => return MalError.TypeError,
    };
    const hmap_cpy = hmap.deepcopy(Allocator, base_hmap) catch return MalError.SystemError;
    new_mal.data = MalData {.HashMap = hmap_cpy};

    const assoc_arr = args_arr[1..args_arr.len];
    if((assoc_arr.len % 2) != 0) return MalError.ArgError;
    var i: usize = 0;
    while(2*i+1 < assoc_arr.len) {
        const this_key = switch(assoc_arr[2*i].data) {
            .String => |s| s,
            .Keyword => |kwd| kwd,
            else => return MalError.ArgError,
        };
        const this_key_cpy = string_copy(Allocator, this_key) catch return MalError.SystemError;
        const this_val_cpy = try assoc_arr[2*i+1].copy(Allocator);
        try new_mal.hashmap_insert(this_key_cpy, this_val_cpy);
        i += 1;
    }
    return new_mal;
}

pub fn hash_map_dissoc(args: MalLinkedList) MalError!*MalType {
    const args_arr = args.toSlice();
    if(args_arr.len < 1) return MalError.ArgError;
    const new_mal = try MalType.new_nil(Allocator);
    errdefer new_mal.delete(Allocator);
    const base_hmap = switch(args_arr[0].data) {
        .HashMap => |hm| hm,
        else => return MalError.TypeError,
    };
    const hmap_cpy = hmap.deepcopy(Allocator, base_hmap) catch return MalError.SystemError;
    new_mal.data = MalData {.HashMap = hmap_cpy};

    var i: usize = 1;
    while(i < args_arr.len) {
        const this_key = switch(args_arr[i].data) {
            .String => |s| s,
            .Keyword => |kwd| kwd,
            else => return MalError.ArgError,
        };
        try new_mal.hashmap_remove(this_key);
        i += 1;
    }
    return new_mal;
}

pub fn hash_map_get(a1: *MalType, a2: *MalType) MalError!*MalType {
    const key = switch(a2.data) {
        .String => |s| s,
        .Keyword => |kwd| kwd,
        else => return MalError.TypeError,
    };
    const optional_val = try a1.hashmap_get(key);
    if(optional_val) |val| {
        return val.copy(Allocator);
    }
    else return MalType.new_nil(Allocator);
}

pub fn hash_map_contains(a1: *MalType, a2: *MalType) MalError!*MalType {
    const key = switch(a2.data) {
        .String => |s| s,
        .Keyword => |kwd| kwd,
        else => return MalError.TypeError,
    };
    const contains_bool = try a1.hashmap_contains(key);
    return MalType.new_bool(Allocator, contains_bool);
}

pub fn hash_map_keys(a1: *MalType) MalError!*MalType {
    const hm = switch(a1.data) {
        .HashMap => |h| h,
        else => return MalError.TypeError,
    };
    var new_ll = MalLinkedList.init(Allocator);
    errdefer linked_list.destroy(Allocator, &new_ll, false);
    var iterator = hm.iterator();
    var optional_pair = iterator.next();

    while(true) {
        const pair = optional_pair orelse break;
        const key = string_copy(Allocator, pair.key) catch return MalError.SystemError;
        
        var key_mal: *MalType = undefined;
        if(key.len > 1 and key[0] == 255) {
            key_mal = try MalType.new_keyword(Allocator, key[1..key.len]);
        } else {
            key_mal = try MalType.new_string(Allocator, key);
        }
        try linked_list.append_mal(Allocator, &new_ll, key_mal);
        optional_pair = iterator.next();
    }
    var new_mal = try MalType.new_nil(Allocator);
    new_mal.data = MalData{.List = new_ll};
    return new_mal;
}

pub fn hash_map_vals(a1: *MalType) MalError!*MalType {
    const hm = switch(a1.data) {
        .HashMap => |h| h,
        else => return MalError.TypeError,
    };
    var new_ll = MalLinkedList.init(Allocator);
    errdefer linked_list.destroy(Allocator, &new_ll, false);
    var iterator = hm.iterator();
    var optional_pair = iterator.next();

    while(true) {
        const pair = optional_pair orelse break;
        const val = try pair.value.copy(Allocator);
        try linked_list.append_mal(Allocator, &new_ll, val);
        optional_pair = iterator.next();
    }
    var new_mal = try MalType.new_nil(Allocator);
    new_mal.data = MalData{.List = new_ll};
    return new_mal;
}

pub fn sequence_length(a1: *MalType) MalError!*MalType {
    const len = switch(a1.data) {
        .List => |l| l.count(),
        .Vector => |v| v.count(),
        .String => |s| s.len,
        .Nil => 0,
        else => return MalError.TypeError,
    };
    return MalType.new_int(Allocator, @intCast(i64,len));
}

pub fn keyword(a1: *MalType) MalError!*MalType {
    const kwd = switch(a1.data) {
        .String => |s| s,
        .Keyword => |k| return a1.copy(Allocator),
        else => return MalError.TypeError,
    };
    return MalType.new_keyword(Allocator, kwd);
}

pub fn readline(a1: *MalType) MalError!*MalType {
    const prompt = try a1.as_string();
    const optional_read_line = getline_prompt(Allocator, prompt)
        catch return MalError.SystemError;
    if(optional_read_line) |read_line| {
        return MalType.new_string(Allocator, read_line);
    }
    const mal = try MalType.new_nil(Allocator);
    return MalType.new_nil(Allocator);
}

pub fn time_ms() MalError!*MalType {
    const itime: i64 = @intCast(i64, std.time.milliTimestamp());
    return MalType.new_int(Allocator, itime);
}

pub fn meta(a1: *MalType) MalError!*MalType {
    if(a1.meta) |mal_meta| {
        return mal_meta.copy(Allocator);
    }
    return MalType.new_nil(Allocator);
}

pub fn with_meta(a1: *MalType, a2: *MalType) MalError!*MalType {
    var new_mal = try a1.copy(Allocator);
    if(new_mal.meta) |mal_meta| {
        mal_meta.delete(Allocator);
    }
    new_mal.meta = try a2.copy(Allocator);
    return new_mal;
}

pub fn seq(a1: *MalType) MalError!*MalType {
    switch(a1.data) {
        .List => |l| {
            if(l.count() == 0) return MalType.new_nil(Allocator);
            return a1.copy(Allocator);
        },
        .Vector => |v| {
            if(v.count() == 0) return MalType.new_nil(Allocator);
            const mal_copy = try a1.copy(Allocator);
            const ll = mal_copy.data.Vector;
            mal_copy.data = MalData{.List = ll};
            return mal_copy;
        },
        .String => |s| {
            if(s.len == 0) return MalType.new_nil(Allocator);
            const new_list = try MalType.new_list_empty(Allocator);
            for(s) |letter| {
                const new_char = try MalType.new_string(Allocator, [_]u8 {letter});
                try new_list.sequence_append(Allocator, new_char);
            }
            return new_list;
        },
        .Nil => {
            return MalType.new_nil(Allocator);
        },
        else => {
            return MalError.TypeError;
        }
    }
    return MalType.new_nil(Allocator);
}

pub fn conj(args: MalLinkedList) MalError!*MalType {
    var iterator = args.iterator();
    const container = iterator.next() orelse return MalError.ArgError;
    const append = switch(container.data) {
        .List => false,
        .Vector => true,
        else => return MalError.ArgError,
    };

    var return_mal = try container.copy(Allocator);    
    while(iterator.next()) |mal| {
        const mal_copy = try mal.copy(Allocator);
        if(append) {
            try return_mal.sequence_append(Allocator, mal_copy);
        } else {
            try return_mal.sequence_prepend(Allocator, mal_copy);
        }
    }
    return return_mal;
}

fn read_string(a1: *MalType) MalError!*MalType {
    const str_to_eval = try a1.as_string();
    var read = try reader.read_str(str_to_eval);
    return (try reader.read_form(&read)) orelse return MalType.new_nil(Allocator);
}

pub fn do_apply(args: MalLinkedList) MalError!*MalType {
    // TODO: not always safe to delete new_ll here
    if(args.count() == 0) return MalError.ArgError;
    var args_copy = args;
    const list_node = args_copy.pop();
    const list_ll = try list_node.sequence_linked_list();
    var new_ll = try linked_list.deepcopy(Allocator, list_ll.*);
    defer linked_list.destroy(Allocator, &new_ll, false);
    var optional_node = args_copy.popOrNull();
    while(optional_node) |node| {
        try linked_list.prepend_mal(Allocator, &new_ll, try node.copy(Allocator));
        optional_node = args_copy.popOrNull();
    }
    var return_mal = apply_function(Allocator, new_ll);
    return return_mal;
}

pub const CorePairType = enum {
    Fn0,
    Fn1,
    Fn2,
    Fn3,
    Fn4,
    FVar,
};

pub const CorePairData = union(CorePairType) {
    Fn0: *const fn() MalError!*MalType,
    Fn1: *const fn(a1: *MalType) MalError!*MalType,
    Fn2: *const fn(a1: *MalType, a2: *MalType) MalError!*MalType,
    Fn3: *const fn(a1: *MalType, a2: *MalType, a3: *MalType) MalError!*MalType,
    Fn4: *const fn(a1: *MalType, a2: *MalType, a3: *MalType, a4: *MalType) MalError!*MalType,    
    FVar: *const fn(args: MalLinkedList) MalError!*MalType,
};

pub const CorePair = struct {
    name: []const u8,
    func: CorePairData,
};

pub const core_namespace = [_] CorePair {
    CorePair { .name = "+", .func = CorePairData {.Fn2 = &int_plus} },
    CorePair { .name = "-", .func = CorePairData {.Fn2 = &int_minus} },
    CorePair { .name = "*", .func = CorePairData {.Fn2 = &int_mult} },
    CorePair { .name = "/", .func = CorePairData {.Fn2 = &int_div} },
    CorePair { .name = "<",  .func = CorePairData {.Fn2 = &int_lt} },
    CorePair { .name = "<=", .func = CorePairData {.Fn2 = &int_leq} },
    CorePair { .name = ">",  .func = CorePairData {.Fn2 = &int_gt} },
    CorePair { .name = ">=", .func = CorePairData {.Fn2 = &int_geq} },
    CorePair { .name = "=",  .func = CorePairData {.Fn2 = &equality} },
    CorePair { .name = "list?", .func = CorePairData {.Fn1 = &is_list} },
    CorePair { .name = "vector?", .func = CorePairData {.Fn1 = &is_vector} },
    CorePair { .name = "count", .func = CorePairData {.Fn1 = &sequence_length} },
    CorePair { .name = "list",  .func = CorePairData {.FVar = &list} },
    CorePair { .name = "vector",  .func = CorePairData {.FVar = &vector} },
    CorePair { .name = "map",  .func = CorePairData {.FVar = &map} },
    CorePair { .name = "empty?", .func = CorePairData {.Fn1 = &empty} },
    CorePair { .name = "prn", .func = CorePairData {.FVar = &prn} },
    CorePair { .name = "println", .func = CorePairData {.FVar = &println} },
    CorePair { .name = "pr-str", .func = CorePairData {.FVar = &pr_str} },
    CorePair { .name = "str", .func = CorePairData {.FVar = &str} },
    CorePair { .name = "slurp", .func = CorePairData {.Fn1 = &slurp} },
    CorePair { .name = "atom", .func = CorePairData {.Fn1 = &atom} },
    CorePair { .name = "atom?", .func = CorePairData {.Fn1 = &is_atom} },
    CorePair { .name = "deref", .func = CorePairData {.Fn1 = &deref} },
    CorePair { .name = "reset!", .func = CorePairData {.Fn2 = &atom_reset} },
    CorePair { .name = "swap!", .func = CorePairData {.FVar = &atom_swap} },
    CorePair { .name = "cons", .func = CorePairData {.Fn2 = &cons} },
    CorePair { .name = "concat", .func = CorePairData {.FVar = &concat} },
    CorePair { .name = "rest", .func = CorePairData {.Fn1 = &rest } },
    CorePair { .name = "nth", .func = CorePairData {.Fn2 = &nth } },
    CorePair { .name = "first", .func = CorePairData {.Fn1 = &first } },
    CorePair { .name = "nil?", .func = CorePairData {.Fn1 = &is_nil } },
    CorePair { .name = "true?", .func = CorePairData {.Fn1 = &is_true } },
    CorePair { .name = "false?", .func = CorePairData {.Fn1 = &is_false } },
    CorePair { .name = "symbol", .func = CorePairData {.Fn1 = &symbol } },
    CorePair { .name = "symbol?", .func = CorePairData {.Fn1 = &is_symbol } },
    CorePair { .name = "keyword?", .func = CorePairData {.Fn1 = &is_keyword } },
    CorePair { .name = "map?", .func = CorePairData {.Fn1 = &is_map } },
    CorePair { .name = "sequential?", .func = CorePairData {.Fn1 = &is_sequential } },
    CorePair { .name = "apply", .func = CorePairData {.FVar = &do_apply } },
    CorePair { .name = "hash-map", .func = CorePairData {.FVar = &hash_map } },
    CorePair { .name = "assoc", .func = CorePairData {.FVar = &hash_map_assoc } },
    CorePair { .name = "dissoc", .func = CorePairData {.FVar = &hash_map_dissoc } },
    CorePair { .name = "get", .func = CorePairData {.Fn2 = &hash_map_get } },
    CorePair { .name = "contains?", .func = CorePairData {.Fn2 = &hash_map_contains } },
    CorePair { .name = "keys", .func = CorePairData {.Fn1 = &hash_map_keys } },
    CorePair { .name = "vals", .func = CorePairData {.Fn1 = &hash_map_vals } },
    CorePair { .name = "keyword", .func = CorePairData {.Fn1 = &keyword } },
    CorePair { .name = "read-string", .func = CorePairData {.Fn1 = &read_string } },
    CorePair { .name = "readline", .func = CorePairData {.Fn1 = &readline } },
    CorePair { .name = "time-ms", .func = CorePairData {.Fn0 = &time_ms } },
    CorePair { .name = "meta", .func = CorePairData {.Fn1 = &meta } },
    CorePair { .name = "with-meta", .func = CorePairData {.Fn2 = &with_meta } },
    CorePair { .name = "fn?", .func = CorePairData {.Fn1 = &is_fn } },
    CorePair { .name = "string?", .func = CorePairData {.Fn1 = &is_string } },
    CorePair { .name = "number?", .func = CorePairData {.Fn1 = &is_number } },
    CorePair { .name = "macro?", .func = CorePairData {.Fn1 = &is_macro } },
    CorePair { .name = "seq", .func = CorePairData {.Fn1 = &seq } },
    CorePair { .name = "conj", .func = CorePairData {.FVar = &conj } },
};
