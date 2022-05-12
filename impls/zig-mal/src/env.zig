const std = @import("std");
const Allocator = std.mem.Allocator;

const MalType = @import("./types.zig").MalType;

pub const Env = struct {
    const Key = []const u8;
    const Value = MalType;
    // TODO: use AutoHashMap or other HashMap variant?
    const Data = std.StringHashMap(*MalType);

    const Self = @This();

    outer: ?*const Self,
    data: Data,
    children: std.ArrayList(*Self),

    pub fn init(allocator: Allocator, outer: ?*const Env) Self {
        return .{
            .outer = outer,
            .data = Data.init(allocator),
            .children = std.ArrayList(*Self).init(allocator),
        };
    }

    pub fn initCapacity(allocator: Allocator, outer: ?*const Env, size: u32) !Self {
        var self = Self.init(allocator, outer);
        try self.data.ensureTotalCapacity(size);
        return self;
    }

    pub fn initBindExprs(allocator: Allocator, outer: ?*const Env, binds: []const Key, exprs: []*MalType) !Self {
        std.debug.assert(binds.len == exprs.len);
        var self = try Self.initCapacity(allocator, outer, @intCast(u32, binds.len));
        for (binds) |symbol, i| {
            try self.set(symbol, exprs[i]);
        }
        return self;
    }

    pub fn deinit(self: *Self) void {
        for (self.children.items) |child| {
            child.deinit();
            self.data.allocator.destroy(child);
        }
        self.children.deinit();
        var it = self.data.iterator();
        while (it.next()) |entry| {
            // free copied hash map keys
            self.data.allocator.free(entry.key_ptr.*);
        }
        self.data.deinit();
        self.* = undefined;
    }

    pub fn initChild(self: *Self) !*Self {
        const allocator = self.data.allocator;
        var child_ptr = try allocator.create(Self);
        child_ptr.* = Self.init(allocator, self);
        try self.children.append(child_ptr);
        return child_ptr;
    }

    pub fn initChildBindExprs(self: *Self, binds: []const Key, exprs: []*MalType) !*Self {
        const allocator = self.data.allocator;
        var child_ptr = try allocator.create(Self);
        child_ptr.* = try Self.initBindExprs(allocator, self, binds, exprs);
        try self.children.append(child_ptr);
        return child_ptr;
    }

    pub fn set(self: *Self, symbol: Key, value: *MalType) !void {
        const allocator = self.data.allocator;

        const get_or_put = try self.data.getOrPut(symbol);
        if (get_or_put.found_existing) {
            get_or_put.value_ptr.*.deinit();
        } else {
            // copy the symbol to use as key with the same lifetime as the hash map
            get_or_put.key_ptr.* = allocator.dupe(u8, symbol) catch |err| {
                _ = self.data.remove(symbol);
                return err;
            };
        }
        // TODO: check this
        get_or_put.value_ptr.* = value;
    }

    pub fn find(self: *const Self, symbol: Key) ?*const Self {
        return if (self.data.contains(symbol))
            self
        else if (self.outer) |outer| outer.find(symbol) else null;
    }

    pub fn get(self: Self, symbol: Key) !*MalType {
        return if (self.find(symbol)) |env| env.data.get(symbol) orelse unreachable else error.EnvSymbolNotFound;
    }
};
