const warn = @import("std").log.warn;
const allocator = @import("std").heap.c_allocator;

const hash_map = @import("std").hash_map;
const MalType = @import("types.zig").MalType;
const string_eql = @import("std").hash_map.eqlString;
const MalError = @import("error.zig").MalError;
const debug_alloc = @import("types.zig").debug_alloc;

const Context = struct {

    pub fn hash(_: @This(), key: *MalType) u64 {
        return switch(key.*) {
            .Symbol, .String, .Keyword => |s| hash_map.hashString(s.data),
            else                       => unreachable,
        };
    }

    pub fn eql(_: @This(), ma: *MalType, mb: *MalType) bool {
        return switch(ma.*) {
            .Keyword => |a| switch(mb.*) {
                .Keyword => |b| string_eql(a.data, b.data),
                else     =>    false,
            },
            .String => |a| switch(mb.*) {
                .String => |b| string_eql(a.data, b.data),
                else    =>     false,
            },
            .Symbol => |a| switch(mb.*) {
                .Symbol => |b| string_eql(a.data, b.data),
                else    =>     false,
            },
            else        =>     unreachable,
        };
    }
};


pub const MalHashMap = hash_map.HashMapUnmanaged(*MalType, *MalType,
                                                 Context, 80);

pub fn map_destroy(hashmap: *MalHashMap) void {
    if (debug_alloc) {
        warn("destroy_map_elements", .{});
    }
    var iterator = hashmap.iterator();
    while(iterator.next()) |pair| {
        pair.key_ptr.*.decref();
        pair.value_ptr.*.decref();
    }
    hashmap.deinit(allocator);
}

// If the key was present in the map, the implementation reuses it,
// instead of the new one.  So we need to increment the reference
// counting for the key here.
// The ref count of the value is not incremented here.
pub fn map_insert_incref_key(hashmap: *MalHashMap, key: *MalType, value: *MalType) !void {
    switch(key.*) {
        .String, .Keyword, .Symbol => {
            if (try hashmap.fetchPut(allocator, key, value)) |old| {
                // No change in the key reference count.
                old.value.decref();
            } else {
                key.incref();
            }
        },
        else => return MalError.TypeError,
    }
}

pub fn map_insert_from_map(hashmap: *MalHashMap, from: MalHashMap) !void {
    var iterator = from.iterator();
    while(iterator.next()) |pair| {
        const key = pair.key_ptr.*;
        const value = pair.value_ptr.*;
        try map_insert_incref_key(hashmap, key, value);
        value.incref();
    }
}

pub fn map_insert_from_kvs(hashmap: *MalHashMap, kvs: []const *MalType) !void {
    if (kvs.len % 2 == 1) {
        return MalError.TypeError;
    }
    for (0..kvs.len/2) |i| {
        const key = kvs[2*i];
        const value = kvs[2*i+1];
        try map_insert_incref_key(hashmap, key, value);
        value.incref();
    }
}
