const warn = @import("std").debug.warn;
const Allocator = @import("std").mem.Allocator;

const hash_map = @import("std").hash_map;
const MalType = @import("types.zig").MalType;
const string_eql = @import("utils.zig").string_eql;
const string_copy = @import("utils.zig").string_copy;
const MalError = @import("error.zig").MalError;

fn bad_hash(str: []const u8) u32 {
    var hash: u64 = 1;
    const m: u32 = (1<<31);
    const a: u32 = 1103515245;
    const c: u32 = 12345;

    var i: usize = 0;
    const n = str.len;
    while(i < n) {
        hash = (hash + str[i]) % m;
        hash = (a * hash) % m;
        hash = (c + hash) % m;
        i += 1;
    }
    const res: u32 = @intCast(u32, hash % m);
    return res;
}

pub const MalHashMap = hash_map.HashMap([]const u8, *MalType, bad_hash, string_eql);

pub fn deepcopy(allocator: *Allocator, hashmap: MalHashMap) MalError!MalHashMap {
    var hmap_cpy = MalHashMap.init(allocator);
    var iterator = hashmap.iterator();
    var optional_pair = iterator.next();
    while(true) {
        const pair = optional_pair orelse break;
        const key = string_copy(allocator, pair.key) catch return MalError.SystemError;
        const val = try pair.value.copy(allocator);
        _ = hmap_cpy.put(key, val) catch return MalError.SystemError;
        optional_pair = iterator.next();
    }
    return hmap_cpy;
}

pub fn destroy(allocator: *Allocator, hashmap: MalHashMap, shallow: bool) void {
    var iterator = hashmap.iterator();
    var optional_pair = iterator.next();
    while(true) {
        const pair = optional_pair orelse break;
        //warn(" deleting {} {}\n", pair.key, pair.value);
        if(!shallow) {
            allocator.free(pair.key);
            pair.value.delete(allocator);
        }
        optional_pair = iterator.next();
    }
    hashmap.deinit();
}

