const std = @import("std");
const Allocator = std.mem.Allocator;
const warn = @import("std").debug.warn;

pub const LoggingAllocator = struct {
    allocator: Allocator,
    parent_allocator: *Allocator,

    const Self = @This();

    pub fn init(parent_allocator: *Allocator) Self {
        return Self {
            .allocator = Allocator{
                .reallocFn = realloc,
                .shrinkFn = shrink,
            },
            .parent_allocator = parent_allocator,
        };
    }

    fn realloc(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) ![]u8 {
        warn("mem new {} {} {}\n", old_mem.len, new_size, @intCast(i64,new_size) - @intCast(i64, old_mem.len));
        const self = @fieldParentPtr(Self, "allocator", allocator);
        const result = self.parent_allocator.reallocFn(self.parent_allocator, old_mem, old_align, new_size, new_align);
        return result;
    }

    fn shrink(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) []u8 {
        warn("mem del {} {} {}\n", old_mem.len, new_size, @intCast(i64,new_size) - @intCast(i64,old_mem.len));
        warn("deleted: {}\n", old_mem);
        const self = @fieldParentPtr(Self, "allocator", allocator);
        const result = self.parent_allocator.shrinkFn(self.parent_allocator, old_mem, old_align, new_size, new_align);
        return result;
    }
};
