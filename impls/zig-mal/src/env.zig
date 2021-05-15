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
        const allocator = self.data.allocator;
        var it = self.data.iterator();
        while (it.next()) |entry| {
            // free copied hash map keys and values
            allocator.free(entry.key);
            entry.value.deinit(allocator);
        }
        self.data.deinit();
        self.* = undefined;
    }

    pub fn set(self: *Self, symbol: Key, value: MalValue) !void {
        const allocator = self.data.allocator;

        // if needed copy the value to have the same lifetime as the hash map
        const value_copy = try value.copy(allocator);
        errdefer value_copy.deinit(allocator);

        const get_or_put = try self.data.getOrPut(symbol);
        if (get_or_put.found_existing) {
            get_or_put.entry.value.deinit(allocator);
        } else {
            // copy the symbol to use as key with the same lifetime as the hash map
            get_or_put.entry.key = allocator.dupe(u8, symbol) catch |err| {
                _ = self.data.remove(symbol);
                return err;
            };
        }
        get_or_put.entry.value = value_copy;
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
