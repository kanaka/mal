const std = @import("std");

const Allocator = std.heap.c_allocator;

const MalType = @import("types.zig").MalType;
const printer = @import("printer.zig");
const reader = @import("reader.zig");
const getline_prompt = @import("readline.zig").getline;
const string_eql = std.hash_map.eqlString;

const MalError = @import("error.zig").MalError;

const hmap = @import("hmap.zig");

const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const MalHashMap = @import("hmap.zig").MalHashMap;

//  Set by the step file at startup.
pub var apply_function: *const fn(f: MalType, args: []*MalType) MalError!*MalType = undefined;

const safeAdd = @import("std").math.add;
const safeSub = @import("std").math.sub;
const safeMul = @import("std").math.mul;
const safeDivFloor = @import("std").math.divFloor;

const stdout_file = std.io.getStdOut();
const throw = @import("error.zig").throw;

fn int_plus(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = try safeAdd(i64, x, y);
    return MalType.new_int(res);
}

fn int_minus(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = try safeSub(i64, x, y);
    return MalType.new_int(res);
}

fn int_mult(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = try safeMul(i64, x, y);
    return MalType.new_int(res);
}

fn int_div(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    const x = try a1.as_int();
    const y = try a2.as_int();
    const res = try safeDivFloor(i64, x, y);
    return MalType.new_int(res);
}

fn int_lt(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    return MalType.new_bool((try a1.as_int()) < (try a2.as_int()));
}

fn int_leq(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    return MalType.new_bool((try a1.as_int()) <= (try a2.as_int()));
}

fn int_gt(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    return MalType.new_bool((try a1.as_int()) > (try a2.as_int()));
}

fn int_geq(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    return MalType.new_bool((try a1.as_int()) >= (try a2.as_int()));
}

fn _linked_list_equality(l1: []const *MalType, l2:[]const  *MalType) bool {
    if(l1.len != l2.len) return false;
    for(l1, l2) |m1, m2| {
        if(! _equality(m1.*, m2.*)) {
            return false;
        }
    }
    return true;
}

fn _hashmap_equality(h1: MalHashMap, h2: MalHashMap) bool {
    if(h1.count() != h2.count()) {
        return false;
    }

    var iterator = h1.iterator();
    while(iterator.next()) |pair| {
        const optional_val = h2.get(pair.key_ptr.*);
        if(optional_val) |val| {
            const el_cmp = _equality(pair.value_ptr.*.*, val.*);
            if(! el_cmp) {
                return false;
            }
        }
        else {
            return false;
        }
    }
    return true;
}

fn equality(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    return MalType.new_bool(_equality(a1.*, a2.*));
}

fn _equality(a1: MalType, a2: MalType) bool {
    switch(a1) {
        .Nil => {
            switch(a2) {
                .Nil => return true,
                else => return false,
            }
        },
        .False => {
            switch(a2) {
                .False => return true,
                else => return false,
            }
        },
        .True => {
            switch(a2) {
                .True => return true,
                else => return false,
            }
        },
        .Int => |l1| {
            switch(a2) {
                .Int => |l2| return l1.data == l2.data,
                else => return false,
            }
        },
        .String => |s1| {
            switch(a2) {
                .String => |s2| return string_eql(s1.data, s2.data),
                else => return false,
            }
        },
        .Symbol => |s1| {
            switch(a2) {
                .Symbol => |s2| return string_eql(s1.data, s2.data),
                else => return false,
            }
        },
        .Keyword => |s1| {
            switch(a2) {
                .Keyword => |s2| return string_eql(s1.data, s2.data),
                else => return false,
            }
        },
        .List, .Vector => |l1| {
            switch(a2) {
                .List, .Vector => |l2| return _linked_list_equality(
                    l1.data.items, l2.data.items),
                else => return false,
            }
        },
        .HashMap => |h1| {
            switch(a2) {
                .HashMap => |h2| return _hashmap_equality(h1.data, h2.data),
                else => return false,
            }
        },
        else => {
            return false;
        },
    }
}

fn list(args: []*MalType) !*MalType {
    const new_mal = try MalType.new_list();
    errdefer new_mal.decref();
    for(args) |x| {
        try new_mal.List.data.append(Allocator, x);
        x.incref();
    }
    return new_mal;
}

fn vector(args: []*MalType) !*MalType {
    const new_mal = try MalType.new_vector();
    errdefer new_mal.decref();
    for(args) |x| {
        try new_mal.Vector.data.append(Allocator, x);
        x.incref();
    }
    return new_mal;
}

fn map(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const func_mal = args[0];
    const args_mal = args[1];
    var to_map_ll = try args_mal.as_slice();
    const new_list = try MalType.new_list();
    errdefer new_list.decref();
    for(0..to_map_ll.len) |i| {
        const new_mal = try apply_function(func_mal.*, to_map_ll[i..i+1]);
        try new_list.List.data.append(Allocator, new_mal);
    }
    return new_list;
}

fn is_list(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .List => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_vector(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .Vector => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_string(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .String => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_number(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .Int => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_fn(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const is_function = switch(a1.*) {
        .FnCore => true,
        .Func => |func_data| ! func_data.is_macro,
        else => false,
    };
    return MalType.new_bool(is_function);
}

fn is_macro(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const is_func_and_macro = switch(a1.*) {
        .Func => |data| data.is_macro,
        else => false,
    };
    return MalType.new_bool(is_func_and_macro);
}

fn empty(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const slice = try a1.as_slice();
    return MalType.new_bool(slice.len == 0);
}

fn prn(args: []*MalType) MalError!*MalType {
    try printer.n_stdout(args, true, true);
    try stdout_file.writeAll("\n");
    const mal = &MalType.NIL;
    return mal;
}

fn println(args: []*MalType) !*MalType {
    try printer.n_stdout(args, false, true);
    try stdout_file.writeAll("\n");
    const mal = &MalType.NIL;
    return mal;
}

fn str(args: []*MalType) !*MalType {
    const items = try printer.print_mal_to_string(args, false, false);
    errdefer Allocator.free(items);
    return MalType.new_string(items, false);
}

fn pr_str(args: []*MalType) !*MalType {
    const s = try printer.print_mal_to_string(args, true, true);
    errdefer Allocator.free(s);
    return MalType.new_string(s, false);
}

fn slurp(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const path = try a1.as_string();
    const dir = std.fs.cwd();
    const items = try dir.readFileAlloc(Allocator, path, 10000);
    return MalType.new_string(items, false);
}

fn atom(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const result = try MalType.new_atom(a1);
    a1.incref();
    return result;
}

fn is_atom(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    return switch(args[0].*) {
        .Atom => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn deref(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    switch(a1.*) {
        .Atom => |atom_val| {
            atom_val.data.incref();
            return atom_val.data;
        },
        else => return MalError.TypeError,
    }
}

fn atom_reset(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    switch(a1.*) {
        .Atom => |*atom_val| {
            atom_val.data.decref();
            atom_val.data = a2;
            //  incref for the atom and for the result
            a2.incref();
            a2.incref();
            return a2;
        },
        else => return MalError.TypeError,
    }
}

fn atom_swap(args: []*MalType) !*MalType {
    const n = args.len;
    if(n < 2) return MalError.ArgError;

    const atom_val = switch(args[0].*) {
        .Atom => |*a| a,
        else => return MalError.TypeError,
    };

    var new_args = try Allocator.alloc(*MalType, args.len - 1);
    defer Allocator.free(new_args);
    var i:usize = 0;
    new_args[i] = atom_val.data; i+=1;
    for(args[2..args.len]) |x| {
        new_args[i] = x;
        i += 1;
    }
    std.debug.assert(i == new_args.len);

    const new_mal = try apply_function(args[1].*, new_args);
    atom_val.data.decref(); // after the computation
    atom_val.data = new_mal;
    new_mal.incref();
    return new_mal;
}

fn vec(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    switch(a1.*) {
        .List => |l| {
            const result = try MalType.new_vector();
            errdefer result.decref();
            for(l.data.items) |x| {
                try result.Vector.data.append(Allocator, x);
                x.incref();
            }
            return result;
        },
        .Vector => {
            a1.incref();
            return a1;
        },
        else => return MalError.TypeError,
    }
}

fn cons(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    const old_ll = try a2.as_slice();
    const new_list = try MalType.new_list();
    errdefer new_list.decref();
    try new_list.List.data.append(Allocator, a1);
    a1.incref();
    for(old_ll) |x| {
        try new_list.List.data.append(Allocator, x);
        x.incref();
    }
    return new_list;
}

pub fn concat(args: []*MalType) !*MalType {
    const new_mal = try MalType.new_list();
    errdefer new_mal.decref();
    for(args) |x| {
        for(try x.as_slice()) |y| {
            try new_mal.List.data.append(Allocator, y);
            y.incref();
        }
    }
    return new_mal;
}

fn rest(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const new_mal = try MalType.new_list();
    errdefer new_mal.decref();
    switch(a1.*) {
        .List, .Vector => |l| {
            const old_list = l.data.items;
            if(old_list.len != 0) {
                for(l.data.items[1..]) |x| {
                    try new_mal.List.data.append(Allocator, x);
                    x.incref();
                }
            }
        },
        .Nil => { },
        else => return MalError.TypeError,
    }
    return new_mal;
}

fn nth(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    const l = try a1.as_slice();
    const i = try a2.as_int();
    const pos: usize = @intCast(i);
    if(pos < 0 or l.len <= pos)  {
        return MalError.OutOfBounds;
    }
    const result = l[pos];
    result.incref();
    return result;
}

fn first(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    switch(a1.*) {
        .List, .Vector => |l| {
            if(l.data.items.len == 0) return &MalType.NIL;
            const result = l.data.items[0];
            result.incref();
            return result;
        },
        .Nil => return &MalType.NIL,
        else => return MalError.TypeError,
    }
}

fn is_nil(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .Nil => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_true(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .True => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_false(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .False => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_symbol(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .Symbol => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_keyword(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .Keyword => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_map(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .HashMap => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn is_sequential(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return switch(a1.*) {
        .List, .Vector => &MalType.TRUE,
        else => &MalType.FALSE,
    };
}

fn symbol(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const string = try a1.as_string();
    return MalType.new_symbol(string, true);
}

pub fn hash_map(args: []*MalType) !*MalType {
    const new_mal = try MalType.new_hashmap();
    errdefer new_mal.decref();
    try hmap.map_insert_from_kvs(&new_mal.HashMap.data, args);
    return new_mal;
}

pub fn hash_map_assoc(args: []*MalType) !*MalType {
    if(args.len < 1) return MalError.ArgError;
    const a1 = args[0];
    const new_mal = try MalType.new_hashmap();
    errdefer new_mal.decref();
    const base_hmap = try a1.as_map();
    try hmap.map_insert_from_map(&new_mal.HashMap.data, base_hmap);
    try hmap.map_insert_from_kvs(&new_mal.HashMap.data, args[1..]);
    return new_mal;
}

pub fn hash_map_dissoc(args: []*MalType) !*MalType {
    if(args.len < 1) return MalError.ArgError;
    const a1 = args[0];
    const new_mal = try MalType.new_hashmap();
    errdefer new_mal.decref();
    const base_hmap = try a1.as_map();
    try hmap.map_insert_from_map(&new_mal.HashMap.data, base_hmap);
    for(args[1..]) |k| {
        switch(k.*) {
            .Keyword, .String => {
                if(new_mal.HashMap.data.fetchRemove(k)) |old| {
                    old.key.decref();
                    old.value.decref();
                }
            },
            else => return MalError.TypeError,
        }
    }
    return new_mal;
}

fn hash_map_get(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    const hm = switch(a1.*) {
        .HashMap => |m| m.data,
        .Nil => return &MalType.NIL,
        else => return MalError.TypeError,
    };
    switch(a2.*) {
        .Keyword, .String => {},
        else => return MalError.TypeError,
    }
    if(hm.get(a2)) |value| {
        value.incref();
        return value;
    }
    return &MalType.NIL;
}

fn hash_map_contains(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    switch(a2.*) {
        .Keyword, .String => {
            const hm = try a1.as_map();
            return MalType.new_bool(hm.contains(a2));
        },
        else => return MalError.TypeError,
    }
}

fn hash_map_keys(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const hm = try a1.as_map();
    const new_mal = try MalType.new_list();
    errdefer new_mal.decref();
    var iterator = hm.keyIterator();
    while(iterator.next()) |key_mal| {
        try new_mal.List.data.append(Allocator, key_mal.*);
        key_mal.*.incref();
    }
    return new_mal;
}

fn hash_map_vals(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const hm = try a1.as_map();
    const new_mal = try MalType.new_list();
    errdefer new_mal.decref();
    var iterator = hm.valueIterator();
    while(iterator.next()) |val| {
        try new_mal.List.data.append(Allocator, val.*);
        val.*.incref();
    }
    return new_mal;
}

fn sequence_length(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const len = switch(a1.*) {
        .List, .Vector => |l| l.data.items.len,
        .String => |s| s.data.len,
        .Nil => 0,
        else => return MalError.TypeError,
    };
    return MalType.new_int(@intCast(len));
}

fn keyword(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    switch(a1.*) {
        .String => |s| {
            return MalType.new_keyword(s.data, true);
        },
        .Keyword => {
            a1.incref();
            return a1;
        },
        else => return MalError.TypeError,
    }
}

fn core_readline(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const prompt = try a1.as_string();
    const optional_read_line = try getline_prompt(prompt);
    if(optional_read_line) |read_line| {
        return MalType.new_string(read_line, false);
    }
    return &MalType.NIL;
}

fn time_ms(args: []*MalType) !*MalType {
    if(args.len != 0) return MalError.ArgError;
    const itime = std.time.milliTimestamp();
    return try MalType.new_int(@intCast(itime));
}

fn meta(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const result = switch(a1.*) {
        .List, .Vector => |l| l.metadata,
        .FnCore        => |l| l.metadata,
        .Func          => |l| l.metadata,
        .HashMap       => |l| l.metadata,
        else           => return MalError.TypeError,
    };
    result.incref();
    return result;
}

fn with_meta(args: []*MalType) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const a1 = args[0];
    const a2 = args[1];
    switch(a1.*) {
        .List => |l| {
            const new_mal = try MalType.new_list();
            errdefer new_mal.decref();
            for(l.data.items) |x| {
                try new_mal.List.data.append(Allocator, x);
                x.incref();
            }
            new_mal.List.metadata = a2;
            a2.incref();
            return new_mal;
        },
        .Vector => |l| {
            const new_mal = try MalType.new_vector();
            errdefer new_mal.decref();
            for(l.data.items) |x| {
                try new_mal.Vector.data.append(Allocator, x);
                x.incref();
            }
            new_mal.Vector.metadata = a2;
            a2.incref();
            return new_mal;
        },
        .FnCore => |l| {
            const new_mal = try MalType.newFnCore(l.data);
            new_mal.FnCore.metadata = a2;
            a2.incref();
            return new_mal;
        },
        .Func => |l| {
            const new_mal = try MalType.newFunc(l.arg_list, l.body,
                                                l.environment);
            l.arg_list.incref();
            l.body.incref();
            l.environment.incref();
            new_mal.Func.metadata = a2;
            a2.incref();
            return new_mal;
        },
        .HashMap => |l| {
            const new_mal = try MalType.new_hashmap();
            errdefer new_mal.decref();
            try hmap.map_insert_from_map(&new_mal.HashMap.data, l.data);
            new_mal.HashMap.metadata = a2;
            a2.incref();
            return new_mal;
        },
        else => return MalError.TypeError,
    }
}

fn seq(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    switch(a1.*) {
        .List => |l| {
            if(l.data.items.len == 0) return &MalType.NIL;
            a1.incref();
            return a1;
        },
        .Vector => |l| {
            if(l.data.items.len == 0) return &MalType.NIL;
            const mal_copy = try MalType.new_list();
            errdefer mal_copy.decref();
            for(l.data.items) |x| {
                try mal_copy.List.data.append(Allocator, x);
                x.incref();
            }
            return mal_copy;
        },
        .String => |s| {
            if(s.data.len == 0) return &MalType.NIL;
            const new_list = try MalType.new_list();
            errdefer new_list.decref();
            for(s.data) |x| {
                const one_char = try Allocator.alloc(u8, 1);
                one_char[0] = x;
                const new_char = try MalType.new_string(one_char, false);
                errdefer new_char.decref();
                try new_list.List.data.append(Allocator, new_char);
            }
            return new_list;
        },
        .Nil => {
            return &MalType.NIL;
        },
        else => {
            return MalError.TypeError;
        }
    }
}

pub fn conj(args: []*MalType) !*MalType {
    if(args.len == 0) return MalError.ArgError;
    const container = args[0];
    switch(container.*) {
        .List => |l| {
            const return_mal = try MalType.new_list();
            errdefer return_mal.decref();
            for(1..args.len) |j| {
                const new_item = args[args.len-j];
                try return_mal.List.data.append(Allocator, new_item);
                new_item.incref();
            }
            for(l.data.items) |x| {
                try return_mal.List.data.append(Allocator, x);
                x.incref();
            }
            return return_mal;
        },
        .Vector => |l|{
            const return_mal = try MalType.new_vector();
            errdefer return_mal.decref();
            for(l.data.items) |x| {
                try return_mal.Vector.data.append(Allocator, x);
                x.incref();
            }
            for(args[1..]) |x| {
                try return_mal.Vector.data.append(Allocator, x);
                x.incref();
            }
            return return_mal;
        },
        else => return MalError.ArgError,
    }
}

fn read_string(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    const str_to_eval = try a1.as_string();
    var read = try reader.read_str(str_to_eval);
    return reader.read_form(&read);
}

pub fn do_apply(args: []*MalType) !*MalType {
    if(args.len < 2) return MalError.ArgError;
    const a1 = args[0];
    const last = args[args.len - 1];
    const more_args = try last.as_slice();
    var fargs = try Allocator.alloc(*MalType, args.len + more_args.len - 2);
    defer Allocator.free(fargs);
    var i:usize = 0;
    for(args[1..args.len-1]) |x| { fargs[i] = x; i+=1; }
    for(more_args)           |x| { fargs[i] = x; i+=1; }
    std.debug.assert(i == fargs.len);
    return apply_function(a1.*, fargs);
}

pub fn core_throw(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return throw(a1);
}

pub const CorePair = struct {
    name: []const u8,
    func: *const fn(args: []*MalType) MalError!*MalType,
};

pub const core_namespace = [_]CorePair {
    .{ .name = "+", .func = &int_plus },
    .{ .name = "-", .func = &int_minus },
    .{ .name = "*", .func = &int_mult },
    .{ .name = "/", .func = &int_div },
    .{ .name = "<",  .func = &int_lt },
    .{ .name = "<=", .func = &int_leq },
    .{ .name = ">",  .func = &int_gt },
    .{ .name = ">=", .func = &int_geq },
    .{ .name = "=",  .func = &equality },
    .{ .name = "list?", .func = &is_list },
    .{ .name = "vector?", .func = &is_vector },
    .{ .name = "count", .func = &sequence_length },
    .{ .name = "list",  .func = &list, },
    .{ .name = "vector",  .func = &vector, },
    .{ .name = "map",  .func = &map },
    .{ .name = "empty?", .func = &empty },
    .{ .name = "prn", .func = &prn },
    .{ .name = "println", .func = &println },
    .{ .name = "pr-str", .func = &pr_str },
    .{ .name = "str", .func = &str },
    .{ .name = "slurp", .func = &slurp },
    .{ .name = "atom", .func = &atom },
    .{ .name = "atom?", .func = &is_atom },
    .{ .name = "deref", .func = &deref },
    .{ .name = "reset!", .func = &atom_reset },
    .{ .name = "swap!", .func = &atom_swap },
    .{ .name = "vec", .func = &vec },
    .{ .name = "cons", .func = &cons },
    .{ .name = "concat", .func = &concat },
    .{ .name = "rest", .func = &rest },
    .{ .name = "nth", .func = &nth },
    .{ .name = "first", .func = &first },
    .{ .name = "nil?", .func = &is_nil },
    .{ .name = "true?", .func = &is_true },
    .{ .name = "false?", .func = &is_false },
    .{ .name = "symbol", .func = &symbol },
    .{ .name = "symbol?", .func = &is_symbol },
    .{ .name = "keyword?", .func = &is_keyword },
    .{ .name = "map?", .func = &is_map },
    .{ .name = "sequential?", .func = &is_sequential },
    .{ .name = "apply", .func = &do_apply },
    .{ .name = "hash-map", .func = &hash_map },
    .{ .name = "assoc", .func = &hash_map_assoc },
    .{ .name = "dissoc", .func = &hash_map_dissoc },
    .{ .name = "get", .func = &hash_map_get },
    .{ .name = "contains?", .func = &hash_map_contains },
    .{ .name = "keys", .func = &hash_map_keys },
    .{ .name = "vals", .func = &hash_map_vals },
    .{ .name = "keyword", .func = &keyword },
    .{ .name = "read-string", .func = &read_string },
    .{ .name = "readline", .func = &core_readline },
    .{ .name = "time-ms", .func = &time_ms },
    .{ .name = "meta", .func = &meta },
    .{ .name = "with-meta", .func = &with_meta },
    .{ .name = "fn?", .func = &is_fn },
    .{ .name = "string?", .func = &is_string },
    .{ .name = "number?", .func = &is_number },
    .{ .name = "macro?", .func = &is_macro },
    .{ .name = "seq", .func = &seq },
    .{ .name = "conj", .func = &conj },
    .{ .name = "throw", .func = &core_throw },
};
