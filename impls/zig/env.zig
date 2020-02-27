const std = @import("std");
const warn = @import("std").debug.warn;
const Allocator = @import("std").mem.Allocator;

const string_copy = @import("utils.zig").string_copy;
const string_eql = @import("utils.zig").string_eql;
const MalType = @import("types.zig").MalType;
const MalTypeValue = @import("types.zig").MalTypeValue;
const MalHashMap = @import("hmap.zig").MalHashMap;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const MalError = @import("error.zig").MalError;
const linked_list = @import("linked_list.zig");
const hash_map = @import("hmap.zig");

pub const Env = struct {
    outer: ?**Env,
    data: *MalHashMap,
    allocator: *Allocator,
    refcount: *i32,

    pub fn new(allocator: *Allocator, optional_outer: ?*Env) MalError!*Env {
        const env = allocator.create(Env) catch return MalError.SystemError;
        env.refcount = allocator.create(i32) catch return MalError.SystemError;
        env.refcount.* = 1;
        if(optional_outer) |outer| {
            const env_ptr = allocator.create(*Env) catch return MalError.SystemError;
            env_ptr.* = try outer.copy(allocator);
            env.outer = env_ptr;
        } else {
            env.outer = null;
        }
        env.data = allocator.create(MalHashMap) catch return MalError.SystemError;
        env.data.* = MalHashMap.init(allocator);
        env.allocator = allocator;
        return env;
    }

    pub fn copy(env: *Env, allocator: *Allocator) MalError!*Env {
        const new_env = allocator.create(Env) catch return MalError.SystemError;
        new_env.refcount = env.refcount;
        env.refcount.* += 1;
        new_env.outer = env.outer;
        new_env.data = env.data;
        new_env.allocator = allocator;
        return new_env;
    }

    pub fn delete(env: *Env) void {
        env.refcount.* -= 1;
        if(env.refcount.* <= 0) {
            if(env.outer) |*outer| {
                outer.*.*.delete();
                env.allocator.destroy(env.outer.?);
            }
            //env.print_keys();
            hash_map.destroy(env.allocator, env.data.*, false);
            env.allocator.destroy(env.refcount);
            env.allocator.destroy(env.data);
        }
        env.allocator.destroy(env);
    }

    pub fn set(env: *Env, key: []const u8, value: *MalType) MalError!void {
        const optional_prev_mal = env.data.getValue(key);
        if(optional_prev_mal) |prev_mal| {
            prev_mal.delete(env.allocator);
        }
        //warn("Setting {}\n", key);
        const key_copy = string_copy(env.allocator, key) catch return MalError.SystemError;
        _ = env.data.put(key_copy, value) catch return MalError.SystemError;
    }

    pub fn root_set(env: *Env, key: []const u8, value: *MalType) MalError!void {
        var root_env = env;
        while(true) {
            const outer_ptr = root_env.outer orelse break;
            root_env = outer_ptr.*;
        }
        try root_env.set(key, value);
    }

    pub fn find(env: *const Env, key: []const u8) bool {
        const optional_mal = env.data.getValue(key);
        if(optional_mal) |mal| {
            return true;
        }
        if(env.outer) |outer| {
            return outer.*.find(key);
        }
        return false;
    }

    pub fn get(env: *const Env, key: []const u8) MalError!*MalType {
        const optional_mal = env.data.getValue(key);
        if(optional_mal) |mal| {
            //warn("Got for key '{}': {} (me: {})\n", key, mal, @ptrToInt(env));
            return mal;
        }
        if(env.outer) |outer| {
            return outer.*.get(key);
        }
        return MalError.EnvLookupError;
    }

    pub fn set_list(env: *Env, names: MalLinkedList, vals: MalLinkedList) MalError!void {
        var name_arr = names.toSlice();
        var vals_arr = vals.toSlice();
        var i: usize = 0;        
        
        while(i < name_arr.len) {
            const key = try name_arr[i].as_symbol();
            if(!string_eql(key, "&")) {
                try env.set(key, vals_arr[i]);
                i += 1;
                continue;
            }

            // Here we deal with variadic binding
            if(i+1 >= name_arr.len) return MalError.OutOfBounds;
            const var_key = try name_arr[i+1].as_symbol();
            var new_ll = MalLinkedList.init(env.allocator);
            new_ll.appendSlice(vals_arr[i..vals_arr.len]) catch return MalError.SystemError;
            const new_mal = try MalType.new_list(env.allocator, new_ll);
            try env.set(var_key, new_mal);
            return;
        }
    }

    pub fn set_slice(env: *Env, name_arr: []*MalType, vals_arr: []*MalType) MalError!void {
        var i: usize = 0;        
        
        while(i < name_arr.len) {
            const key = try name_arr[i].as_symbol();
            if(!string_eql(key, "&")) {
                try env.set(key, vals_arr[i]);
                i += 1;
                continue;
            }

            // Here we deal with variadic binding
            if(i+1 >= name_arr.len) return MalError.OutOfBounds;
            const var_key = try name_arr[i+1].as_symbol();
            var new_ll = MalLinkedList.init(env.allocator);
            new_ll.appendSlice(vals_arr[i..vals_arr.len]) catch return MalError.SystemError;
            const new_mal = try MalType.new_list(env.allocator, new_ll);
            try env.set(var_key, new_mal);
            return;
        }
    }
    
    pub fn print_keys(env: *Env) void {
        var it = env.data.iterator();
        var optional_pair = it.next();
        while(optional_pair) |pair| {
            warn("{},",pair.key);
            optional_pair = it.next();
        }
        warn("\n");
    }
};
