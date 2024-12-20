const allocator = @import("std").heap.c_allocator;
const ArrayListUnmanaged = @import("std").ArrayListUnmanaged;
const MalType = @import("types.zig").MalType;

//  The name is poorly choosen but historical.

pub const MalLinkedList = ArrayListUnmanaged(*MalType);

pub fn list_destroy(ll: *MalLinkedList) void {
    for(ll.items) |x|
        x.decref();
    ll.deinit(allocator);
}
