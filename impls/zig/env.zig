const std = @import("std");
const warn = std.log.warn;
const allocator = std.heap.c_allocator;

const MalType = @import("types.zig").MalType;
const MalHashMap = @import("hmap.zig").MalHashMap;
const MalError = @import("error.zig").MalError;
const hash_map = @import("hmap.zig");
const debug_alloc = @import("types.zig").debug_alloc;

pub const Env = struct {
    outer: ?*Env,
    data: MalHashMap,
    refcount: i32 = 1,

    pub fn new_root() Env {
        return .{.outer = null, .data = .{}};
    }

    pub fn new(outer: *Env) !*Env {
        //  The caller is in charge of incremeting the reference count
        //  for outer if necessary.
        const env = try allocator.create(Env);
        env.* = .{ .outer = outer, .data = .{} };
        if(debug_alloc) warn("Env: new {any}", .{env});
        return env;
    }

    pub fn incref(env: *Env) void {
        if(debug_alloc) {
            warn("Env: incref {any}", .{env});
        }
        env.refcount += 1;
        // std.debug.assert(env.refcount < 100);
    }

    pub fn decref(env: *Env) void {
        var e = env;
        while (true) {
            if(debug_alloc) {
                warn("Env: decref {any}", .{e});
                e.print_keys();
            }
            std.debug.assert (0 < e.refcount);
            e.refcount -= 1;
            if(0 < e.refcount) {
                break;
            }
            if(debug_alloc) {
                warn("Env: FREE {any}", .{e});
            }
            const old = e;
            if(e.outer) |outer| {
                e = outer;
            } else {
                warn("INTERNAL ERROR: repl-env should never reach a 0 refcount.", .{});
                break;
            }
            hash_map.map_destroy(&old.data);
            allocator.destroy(old);
        }
    }

    //  Incref both the key and value.
    pub fn set(env: *Env, key: *MalType, value: *MalType) !void {
        //  The caller is in charge of incremeting the reference count
        //  for the value if necessary.
        switch (key.*) {
            .Symbol => {
                if(debug_alloc) {
                    warn("Env: set {s} {any}", .{key.Symbol.data, key});
                }
                try hash_map.map_insert_incref_key(&env.data, key, value);
            },
            else => return MalError.ArgError,
        }
    }

    pub fn get(env: Env, key: *MalType) !?*MalType {
        // The result is not increfed().
        switch (key.*) {
            .Symbol => {
                if(debug_alloc) {
                    warn("Env: get {s} {any}", .{key.Symbol.data, key});
                }
                var e: * const Env = &env;
                while(true) {
                    if(e.data.get(key)) |value| {
                        return value;
                    }
                    e = e.outer orelse return null;
                }
            },
            else => return MalError.KeyError,
        }
    }
    
    pub fn print_keys(env: Env) void {
        var it = env.data.keyIterator();
        var count: i32 = 5;
        while (it.next()) |key| {
            warn("  key={s},", .{key.*.Symbol.data});
            count -= 1;
            if(count <= 0) {
                warn("  ...", .{});
                break;
            }
        }
    }
};
