const warn = @import("std").debug.warn;
const Allocator = @import("std").mem.Allocator;

const hash_map = @import("std").hash_map;
const MalType = @import("types.zig").MalType;
const string_eql = @import("utils.zig").string_eql;
const string_copy = @import("utils.zig").string_copy;
const MalError = @import("error.zig").MalError;

pub const MalHashMap = hash_map.StringHashMap(*MalType);

pub fn deepcopy(allocator: *Allocator, hashmap: MalHashMap) MalError!MalHashMap {
    var hmap_cpy = MalHashMap.init(allocator);
    var iterator = hashmap.iterator();
    var optional_pair = iterator.next();
    while(true) {
        const pair = optional_pair orelse break;
        const key = string_copy(allocator, pair.key_ptr.*) catch return MalError.SystemError;
        const val = try pair.value_ptr.*.copy(allocator);
        _ = hmap_cpy.put(key, val) catch return MalError.SystemError;
        optional_pair = iterator.next();
    }
    return hmap_cpy;
}

pub fn destroy(allocator: *Allocator, hashmap: *MalHashMap, shallow: bool) void {
    var iterator = hashmap.*.iterator();
    var optional_pair = iterator.next();
    while(true) {
        const pair = optional_pair orelse break;
        //warn(" deleting {} {}\n", pair.key, pair.value);
        if(!shallow) {
            allocator.free(pair.key_ptr.*);
            pair.value_ptr.*.delete(allocator);
        }
        optional_pair = iterator.next();
    }
    hashmap.*.deinit();
}

