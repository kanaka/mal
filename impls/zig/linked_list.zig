const Allocator = @import("std").mem.Allocator;

const TailQueue = @import("std").TailQueue;
const ArrayList = @import("std").ArrayList;
const MalType = @import("types.zig").MalType;
const MalError = @import("error.zig").MalError;

pub const MalLinkedList = ArrayList(*MalType);

pub fn deepcopy(allocator: *Allocator, ll: MalLinkedList) MalError!MalLinkedList {
    var new_ll = MalLinkedList.init(allocator);
    const ll_slice = ll.toSlice();
    var i: usize = 0;
    while(i < ll_slice.len) {
        const new_mal = try ll_slice[i].copy(allocator);
        new_ll.append(new_mal) catch return MalError.SystemError;
        i += 1;
    }

    return new_ll;
}

pub fn destroy(allocator: *Allocator, ll: *MalLinkedList, shallow: bool) void {
    if(!shallow) {
        const ll_slice = ll.toSlice();
        var i: usize = 0;
        while(i < ll_slice.len) {
            ll_slice[i].delete(allocator);
            i += 1;
        }
    }
    ll.deinit();
}

// TODO: deprecate
pub fn append_mal(allocator: *Allocator, ll: *MalLinkedList, mal: *MalType) MalError!void {
    ll.append(mal) catch return MalError.SystemError;
}

// TODO: deprecate
pub fn prepend_mal(allocator: *Allocator, ll: *MalLinkedList, mal: *MalType) MalError!void {
    ll.insert(0, mal) catch return MalError.SystemError;
}

pub fn pop_first(allocator: *Allocator, ll: *MalLinkedList) MalError!*MalType {
    if(ll.count() == 0) {
        return MalError.OutOfBounds;
    }
    return ll.orderedRemove(0);
}

pub fn first(ll: *const MalLinkedList) ?*MalType {
    if(ll.count() == 0) {
        return null;
    }
    return ll.at(0);
}

