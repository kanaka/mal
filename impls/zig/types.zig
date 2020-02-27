const string_copy = @import("utils.zig").string_copy;
const string_concat = @import("utils.zig").string_concat;
const Allocator = @import("std").mem.Allocator;
const warn = @import("std").debug.warn;
const Env = @import("env.zig").Env;
const MalError = @import("error.zig").MalError;
const MalHashMap = @import("hmap.zig").MalHashMap;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;

const linked_list = @import("linked_list.zig");
const hash_map = @import("hmap.zig");

pub const MalTypeValue = enum {
    List,
    Vector,
    Generic,
    Int,
    String,
    Keyword,
    Nil,
    True,
    False,
    Fn0,
    Fn1,
    Fn2,
    Fn3,
    Fn4,
    FVar,
    Func,
    Atom,
    HashMap,
};

pub const MalFuncData = struct {
    arg_list: *MalType,
    body: *MalType,
    environment: *Env,
    eval_func: ?(*const fn(o_mal: *MalType, env: *Env) MalError!*MalType),
    is_macro: bool,
};

pub const MalData = union(MalTypeValue) {
    List: MalLinkedList,
    Vector: MalLinkedList,
    Generic: []const u8,
    Int: i64,
    String: []const u8,
    Keyword: []const u8,
    Nil: void,
    True: void,
    False: void,
    Fn0: *const fn () MalError!*MalType,
    Fn1: *const fn (a1: *MalType) MalError!*MalType,
    Fn2: *const fn (a1: *MalType, a2: *MalType) MalError!*MalType,
    Fn3: *const fn (a1: *MalType, a2: *MalType, a3: *MalType) MalError!*MalType,
    Fn4: *const fn (a1: *MalType, a2: *MalType, a3: *MalType, a4: *MalType) MalError!*MalType,
    FVar: *const fn (args: MalLinkedList) MalError!*MalType,
    Func: MalFuncData,
    Atom: **MalType,
    HashMap: MalHashMap,
};

pub const MalType = struct {
    reference_count: *i32,
    data: MalData,
    meta: ?*MalType,

    pub fn new_nil(allocator: *Allocator) MalError!*MalType {
        const mal: *MalType = allocator.create(MalType)
            catch return MalError.SystemError;
        errdefer allocator.destroy(mal);
        mal.reference_count = allocator.create(i32)
            catch return MalError.SystemError;
        mal.reference_count.* = 1;
        mal.data = MalData { .Nil = undefined };
        mal.meta = null;
        return mal;
    }
    
    pub fn new_generic(allocator: *Allocator, value: [] const u8) MalError!*MalType {
        // TODO: should we free on errors?
        const mal: *MalType = try MalType.new_nil(allocator);
        errdefer mal.delete(allocator);
        const value_copy = string_copy(allocator, value)
            catch return MalError.SystemError;
        errdefer allocator.destroy(value_copy);
        mal.data = MalData { .Generic = value_copy };
        return mal;
    }

    pub fn new_string(allocator: *Allocator, value: [] const u8) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        const string_cpy = string_copy(allocator, value) catch return MalError.SystemError;
        mal.data = MalData { .String = string_cpy };
        return mal;
    }

    pub fn new_keyword(allocator: *Allocator, value: [] const u8) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        const kwd_prefix: [] const u8 = [_]u8 {255};
        const kwd_cpy = string_concat(allocator, kwd_prefix, value)
            catch return MalError.SystemError;
        mal.data = MalData { .Keyword = kwd_cpy };
        return mal;
    }
    
    pub fn new_int(allocator: *Allocator, value: i64) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        mal.data = MalData { .Int = value };
        return mal;
    }

    pub fn new_bool(allocator: *Allocator, b: bool) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        if(b) {
            mal.data = MalData { .True = undefined };
        }
        else {
            mal.data = MalData { .False = undefined };
        }
        return mal;
    }

    pub fn new_list_empty(allocator: *Allocator) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        mal.data = MalData {.List = MalLinkedList.init(allocator)};
        return mal;
    }

    pub fn new_vector_empty(allocator: *Allocator) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        mal.data = MalData {.Vector = MalLinkedList.init(allocator)};
        return mal;
    }

    pub fn new_list(allocator: *Allocator, ll: MalLinkedList) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        mal.data = MalData {.List = ll};
        return mal;
    }
    
    pub fn new_vector(allocator: *Allocator, ll: MalLinkedList) MalError!*MalType {
        const mal = try MalType.new_nil(allocator);
        mal.data = MalData {.Vector = ll};
        return mal;
    }

    pub fn new_atom(allocator: *Allocator, mal: *MalType) MalError!*MalType {
        const new_mal = try MalType.new_nil(allocator);
        errdefer new_mal.delete(allocator);
        const atom_value = allocator.create(*MalType) catch return MalError.SystemError;
        atom_value.* = try mal.copy(allocator);
        new_mal.data = MalData { .Atom = atom_value };
        return new_mal;
    }

    pub fn new_hashmap(allocator: *Allocator) MalError!*MalType {
        const new_mal = try MalType.new_nil(allocator);
        errdefer new_mal.delete(allocator);
        const hmap = MalHashMap.init(allocator);
        new_mal.data = MalData {.HashMap = hmap};
        return new_mal;
    }

    pub fn hashmap_insert(mal: *MalType, key: []const u8, value: *MalType) MalError!void {
        switch(mal.data) {
            .HashMap => |*hmap| {
                _ = hmap.*.put(key, value) catch return MalError.SystemError;
            },
            else => return MalError.TypeError,
        }
    }

    pub fn hashmap_remove(mal: *MalType, key: []const u8) MalError!void {
        switch(mal.data) {
            .HashMap => |*hmap| {
                _ = hmap.*.remove(key);
            },
            else => return MalError.TypeError,
        }
    }
    
    pub fn hashmap_get(mal: *MalType, key: []const u8) MalError!?*MalType {
        // TODO: should we copy the data here, or downstream?
        switch(mal.data) {
            .HashMap => |hmap| {
                return hmap.getValue(key);
            },
            .Nil => {
                return null;
            },
            else => return MalError.TypeError,
        }        
    }

    pub fn hashmap_contains(mal: *MalType, key: []const u8) MalError!bool {
        // TODO: should we copy the data here, or downstream?
        return switch(mal.data) {
            .HashMap => |hmap| (hmap.getValue(key) != null),
            else => MalError.TypeError,
        };
    }

    pub fn sequence_linked_list(mal: *MalType) MalError!*MalLinkedList {
        return switch(mal.data) {
            .List => |*l| l,
            .Vector => |*v| v,
            else => MalError.TypeError,
        };
    }

    pub fn const_sequence_linked_list(mal: *const MalType) MalError!MalLinkedList {
        return switch(mal.data) {
            .List => |l| l,
            .Vector => |v| v,
            else => MalError.TypeError,
        };
    }
    
    pub fn sequence_append(mal: *MalType, allocator: *Allocator, new_el: *MalType) MalError!void {
        var ll = try mal.sequence_linked_list();
        try linked_list.append_mal(allocator, ll, new_el);
    }

    pub fn sequence_prepend(mal: *MalType, allocator: *Allocator, new_el: *MalType) MalError!void {
        var ll = try mal.sequence_linked_list();
        try linked_list.prepend_mal(allocator, ll, new_el);
    }
    
    pub fn sequence_pop_first(mal: *MalType, allocator: *Allocator) MalError!*MalType {
        var ll = try mal.sequence_linked_list();
        return linked_list.pop_first(allocator, ll);
    }

    pub fn sequence_pop_last(mal: *MalType, allocator: *Allocator) MalError!*MalType {
        var ll = try mal.sequence_linked_list();
        if(ll.count() == 0) {
            return MalError.OutOfBounds;
        }
        return ll.pop();
    }
    
    pub fn sequence_length(mal: *MalType) MalError!i64 {
        return switch(mal.data) {
            .List => |l| @intCast(i64, l.count()),
            .Vector => |v| @intCast(i64, v.count()),
            else => MalError.TypeError,
        };
    }

    pub fn sequence_nth(mal: *MalType, pos: u32) MalError!*MalType {
        var ll = try mal.sequence_linked_list();
        if(ll.count() <= pos) {
            return MalError.OutOfBounds;
        }
        return ll.at(pos);
    }
    
    pub fn as_int(mal: *const MalType) MalError!i64 {
        return switch(mal.data) {
            .Int => |val| val,
            else => MalError.TypeError,
        };
    }

    pub fn as_symbol(mal: *const MalType) MalError![]const u8 {
        return switch(mal.data) {
            .Generic => |val| val,
            else => MalError.TypeError,
        };        
    }

    pub fn as_string(mal: *const MalType) MalError![]const u8 {
        return switch(mal.data) {
            .String => |s| s,
            else => MalError.TypeError,
        };        
    }

    pub fn shallow_destroy(mal: *MalType, allocator: *Allocator) void {
        mal.reference_count.* -= 1;
        if(mal.meta) |mal_meta| {
            mal_meta.delete(allocator);
        }
        if(mal.reference_count.* <= 0) {
            allocator.destroy(mal.reference_count);
        }
        allocator.destroy(mal);
    }

    pub fn delete(mal: *MalType, allocator: *Allocator) void {
        const ref_count = mal.reference_count.*;
        switch(mal.data) {
            .List => |*l| {
                linked_list.destroy(allocator, l, false);
            },
            .Vector => |*v| {
                linked_list.destroy(allocator, v, false);
            },
            .String => |string| {
                allocator.free(string);
            },
            .Generic => |string| {
                allocator.free(string);
            },
            .Keyword => |string| {
                allocator.free(string);
            },
            .Atom => |atom| {
                if(ref_count <= 1)
                    atom.*.delete(allocator);
            },
            .HashMap => |hm| {
                hash_map.destroy(allocator, hm, false);
            },
            .Func => |func_data| {
                func_data.arg_list.delete(allocator);
                func_data.body.delete(allocator);
                func_data.environment.delete();
            },
            else => {},
        }
        mal.shallow_destroy(allocator);
    }

    pub fn get_num_args(mal: *const MalType) i8 {
        return switch(mal.data) {
            .Fn0 => 0,
            .Fn1 => 1,
            .Fn2 => 2,
            .Fn3 => 3,
            .Fn4 => 4,
            .FVar => -1,
            else => -2,
        };
    }

    pub fn copy(mal: *const MalType, allocator: *Allocator) MalError!*MalType {
        var new_mal = allocator.create(MalType)
            catch |err| return MalError.SystemError;

        new_mal.reference_count = mal.reference_count;
        mal.reference_count.* += 1;
        new_mal.data = MalData {.Nil=undefined};

        if(mal.meta) |mal_meta| {
            new_mal.meta = try mal_meta.copy(allocator);
        } else {
            new_mal.meta = null;
        }

        switch(mal.data) {
            .Generic => |val| {
                const cpy_val = string_copy(allocator, val)
                    catch return MalError.SystemError;
                new_mal.data = MalData { .Generic = cpy_val };
            },
            .Int => |val| {
                new_mal.data = MalData { .Int = val };
            },
            .Fn0 => |f0| {
                new_mal.data = MalData { .Fn0 = f0 };
            },
            .Fn1 => |f1| {
                new_mal.data = MalData { .Fn1 = f1 };
            },
            .Fn2 => |f2| {
                new_mal.data = MalData { .Fn2 = f2 };
            },
            .Fn3 => |f3| {
                new_mal.data = MalData { .Fn3 = f3 };
            },
            .Fn4 => |f4| {
                new_mal.data = MalData { .Fn4 = f4 };
            },
            .FVar => |f| {
                new_mal.data = MalData { .FVar = f };
            },
            .String => |string| {
                const string_cpy = string_copy(allocator, string)
                    catch return MalError.SystemError;
                new_mal.data = MalData { .String = string_cpy };
            },
            .Keyword => |kwd| {
                const kwd_cpy = string_copy(allocator, kwd)
                    catch return MalError.SystemError;
                new_mal.data = MalData { .Keyword = kwd_cpy };
            },
            .List => |l| {
                new_mal.data = MalData { .List = try linked_list.deepcopy(allocator, l) };
            },
            .Vector => |v| {
                new_mal.data = MalData { .Vector = try linked_list.deepcopy(allocator, v) };
            },
            .Func => |func_data| {
                const al = try func_data.arg_list.copy(allocator);
                const b = try func_data.body.copy(allocator);
                const new_func_data = MalFuncData {
                    .arg_list = al,
                    .body = b,
                    .environment = try func_data.environment.copy(allocator),
                    .eval_func = func_data.eval_func,
                    .is_macro = func_data.is_macro,
                };
                new_mal.data = MalData { .Func = new_func_data };
            },
            .Atom => |atom| {
                new_mal.data = MalData { .Atom = atom };
            },
            .HashMap => |h| {
                new_mal.data = MalData {.HashMap = try hash_map.deepcopy(allocator, h)};
            },
            else => {
                new_mal.data = mal.data;
            },
        }
        return new_mal;
    }
};

pub fn apply_function(allocator: *Allocator, args: MalLinkedList) MalError!*MalType {
    // TODO: this should take a MLL pointer
    var args_copy = try linked_list.deepcopy(allocator, args); //TODO: could be more efficient
    var args_arr = args_copy.toSlice();
    const mal_func = args_arr[0];
    
    // First check if it is a user-defined Mal function
    if(MalTypeValue(mal_func.data) == MalTypeValue.Func) {
        const func_data = mal_func.data.Func;
        const args_ll = try func_data.arg_list.sequence_linked_list();
        const func_env = func_data.environment;
        const eval_func = func_data.eval_func orelse return MalError.TypeError;
        var new_env = try Env.new(allocator, func_env);
        // TODO: make sure that set_list checks that first_arg and first_arg_value have same len
        try new_env.set_slice(args_ll.toSlice(), args_arr[1..args_arr.len]);
        
        linked_list.destroy(allocator, &args_copy, true);
        const new_body = try func_data.body.copy(allocator);
        mal_func.delete(allocator);
        return eval_func.*(new_body, new_env);
    }

    // Otherwise, it is a built-in Zig function
    // TODO: safety?
    const n = mal_func.get_num_args();

    if(n <= -2) {
        return MalError.ArgError;
    }

    if(n == -1) {
        // Variable arg function
        (try linked_list.pop_first(allocator, &args_copy)).delete(allocator);
        defer linked_list.destroy(allocator, &args_copy, false);
        return (mal_func.data.FVar.*)(args_copy);
    }

    var arg = args_arr[1..args_arr.len];
    
    // TODO: replace this
    const ret = switch(n) {
        0 => (mal_func.data.Fn0.*)(),
        1 => (mal_func.data.Fn1.*)(arg[0]),
        2 => (mal_func.data.Fn2.*)(arg[0], arg[1]),
        3 => (mal_func.data.Fn3.*)(arg[0], arg[1], arg[2]),
        4 => (mal_func.data.Fn4.*)(arg[0], arg[1], arg[2], arg[3]),
        else => MalError.ArgError,
    };
    linked_list.destroy(allocator, &args_copy, false);
    return ret;
}
