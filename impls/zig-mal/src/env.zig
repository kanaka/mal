const std = @import("std");
const Allocator = std.mem.Allocator;

const MalValue = @import("./types.zig").MalValue;

pub const Env = struct {
    const Key = []const u8;
    const Value = MalValue;
    // TODO: use AutoHashMap or other HashMap variant?
    const Data = std.StringHashMap(MalValue);

    const Self = @This();

    outer: ?*const Self,
    data: Data,

    pub fn init(allocator: Allocator, outer: ?*const Env) Self {
        return .{ .outer = outer, .data = Data.init(allocator) };
    }

    pub fn initCapacity(allocator: Allocator, outer: ?*const Env, size: u32) !Self {
        var self = Self.init(allocator, outer);
        try self.data.ensureTotalCapacity(size);
        return self;
    }

    pub fn initBindExprs(allocator: Allocator, outer: ?*const Env, binds: []const Key, exprs: []const MalValue) !Self {
        std.debug.assert(binds.len == exprs.len);
        var self = try Self.initCapacity(allocator, outer, @intCast(u32, binds.len));
        for (binds) |symbol, i| {
            try self.set(symbol, exprs[i]);
        }
        return self;
    }

    pub fn deinit(self: *Self) void {
        const allocator = self.data.allocator;
        var it = self.data.iterator();
        while (it.next()) |entry| {
            // free copied hash map keys and values
            allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit(allocator);
        }
        self.data.deinit();
        self.* = undefined;
    }

    pub fn deinitAlloc(self: *Self, allocator: Allocator) void {
        var it = self.data.iterator();
        while (it.next()) |entry| {
            // free copied hash map keys and values
            allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit(allocator);
        }
        self.data.deinit();
        allocator.destroy(self);
        self.* = undefined;
    }

    pub fn copy(self: Self, allocator: Allocator) !*Self {
        // const data_copy = try self.data.clone();
        var other_ptr = try allocator.create(Self);
        other_ptr.* = try Self.initCapacity(allocator, self.outer, self.data.unmanaged.size);
        var it = self.data.iterator();
        while (it.next()) |entry| {
            try other_ptr.set(entry.key_ptr.*, entry.value_ptr.*);
        }
        return other_ptr;
    }

    pub fn initChild(self: Self) !*Self {
        const allocator = self.data.allocator;
        var child_ptr = try allocator.create(Self);
        child_ptr.* = Self.init(allocator, &self);
        return child_ptr;
    }

    pub fn initChildBindExprs(self: *const Self, binds: []const Key, exprs: []const MalValue) !*Self {
        const allocator = self.data.allocator;
        var child_ptr = try allocator.create(Self);
        child_ptr.* = try Self.initBindExprs(allocator, self, binds, exprs);
        return child_ptr;
    }

    pub fn set(self: *Self, symbol: Key, value: MalValue) !void {
        const allocator = self.data.allocator;

        // if needed copy the value to have the same lifetime as the hash map
        const value_copy = try value.copy(allocator);
        errdefer value_copy.deinit(allocator);

        const get_or_put = try self.data.getOrPut(symbol);
        if (get_or_put.found_existing) {
            get_or_put.value_ptr.*.deinit(allocator);
        } else {
            // copy the symbol to use as key with the same lifetime as the hash map
            get_or_put.key_ptr.* = allocator.dupe(u8, symbol) catch |err| {
                _ = self.data.remove(symbol);
                return err;
            };
        }
        get_or_put.value_ptr.* = value_copy;
    }

    pub fn find(self: *const Self, symbol: Key) ?*const Self {
        return if (self.data.contains(symbol))
            self
        else if (self.outer) |outer| outer.find(symbol) else null;
    }

    pub fn get(self: Self, symbol: Key) !MalValue {
        return if (self.find(symbol)) |env| env.data.get(symbol) orelse unreachable else error.EnvSymbolNotFound;
    }
};
