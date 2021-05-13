const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("types.zig");
const MalValue = types.MalValue;

pub const Env = struct {
    const Key = []const u8;
    const Value = types.MalValue;
    // TODO: use AutoHashMap or other HashMap variant?
    const Data = std.StringHashMap(MalValue);

    const Self = @This();

    outer: ?*const Self,
    data: Data,

    pub fn init(allocator: *Allocator, outer: ?*const Env) Self {
        return .{ .outer = outer, .data = Data.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        var it = self.data.iterator();
        while (it.next()) |entry| {
            // free copied hash map keys
            self.data.allocator.free(entry.key);
        }
        self.data.deinit();
        self.* = undefined;
    }

    pub fn set(self: *Self, symbol: Key, value: MalValue) !void {
        // copy the symbol to use as key in the environment hash map
        const key = try self.data.allocator.dupe(u8, symbol);
        try self.data.put(key, value);
    }

    pub fn find(self: Self, symbol: Key) ?*const Self {
        return if (self.data.contains(symbol))
            &self
        else if (self.outer) |outer| outer.find(symbol) else null;
    }

    pub fn get(self: Self, symbol: Key) !MalValue {
        return if (self.find(symbol)) |env| env.data.get(symbol) orelse unreachable else error.EnvSymbolNotFound;
    }
};
