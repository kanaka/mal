const std = @import("std");

const allocator = std.heap.c_allocator;
const warn = std.log.warn;
const Env = @import("env.zig").Env;
const MalError = @import("error.zig").MalError;
const MalHashMap = @import("hmap.zig").MalHashMap;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;

const linked_list = @import("linked_list.zig");
const hash_map = @import("hmap.zig");
const map_destroy = @import("hmap.zig").map_destroy;

pub const debug_alloc = false;

pub const ListData = struct {
    data: MalLinkedList,
    reference_count: i32 = 1,
    metadata: *MalType = &MalType.NIL,
};

pub const FnCoreData = struct {
    data: *const fn (args: []*MalType) MalError!*MalType,
    reference_count: i32 = 1,  //  May reach 0 when metadata.
    metadata: *MalType = &MalType.NIL,
};

pub const MalFuncData = struct {
    arg_list: *MalType,
    body: *MalType,
    environment: *Env,
    is_macro: bool = false,
    reference_count: i32 = 1,
    metadata: *MalType = &MalType.NIL,

    pub fn gen_env(self: MalFuncData, args: []*MalType) !*Env {
        const binds = try self.arg_list.as_slice();
        var res = try Env.new(self.environment);
        self.environment.incref();
        errdefer res.decref();
        if (2 <= binds.len
            and std.hash_map.eqlString(binds[binds.len - 2].Symbol.data, "&"))
        {
            if (args.len < binds.len - 2)
                return MalError.TypeError;
            for (binds[0..binds.len-2], args[0..binds.len-2]) |k, v| {
                try res.set(k, v);
                v.incref();
            }
            const more = try MalType.new_list();
            errdefer more.decref();
            for (args[binds.len-2..args.len]) |x| {
                try more.List.data.append(allocator, x);
                x.incref();
            }
            try res.set(binds[binds.len - 1], more);
            //  Do not increment the reference count for this value.
        }
        else {
            if (args.len != binds.len) {
                return MalError.TypeError;
            }
            for(binds, args) |k, v| {
                try res.set(k, v);
                v.incref();
            }
        }
        return res;
    }
};

pub const StringData = struct {
    data: [] const u8,
    reference_count: i32 = 1,
};

pub const HashMapData = struct {
    data: MalHashMap,
    reference_count: i32 = 1,
    metadata: *MalType = &MalType.NIL,
};

pub const MalType = union(enum) {
    List: ListData,
    Vector: ListData,
    Int: struct {
        data: i64,
        reference_count: i32 = 1,
    },
    Symbol: StringData,
    String: StringData,
    Keyword: StringData,
    Nil: void,
    True: void,
    False: void,
    FnCore: FnCoreData,
    Func: MalFuncData,
    Atom: struct {
        data: *MalType,
        reference_count: i32 = 1,
    },
    HashMap: HashMapData,

    //  Define some frequent values in advance.  They are not allocated
    //  on the heap, but should never be deallocated anyway.
    pub var NIL   = MalType { .Nil   = undefined };
    pub var FALSE = MalType { .False = undefined };
    pub var TRUE  = MalType { .True  = undefined };

    pub fn new_symbol(value: []const u8, copy: bool) !*MalType {
        const mal = try allocator.create(MalType);
        errdefer allocator.destroy(mal);
        const data = if (copy) try allocator.dupe(u8, value) else value;
        mal.* = .{.Symbol=.{.data = data}};
        if (debug_alloc) warn("Init {any}", .{mal});
        return mal;
    }

    pub fn new_string(value: []const u8, copy: bool) !*MalType {
        const mal = try allocator.create(MalType);
        errdefer allocator.destroy(mal);
        const data = if (copy) try allocator.dupe(u8, value) else value;
        mal.* = .{.String=.{.data = data}};
        if (debug_alloc) warn("Init {any}", .{mal});
        return mal;
    }

    pub fn new_keyword(value: []const u8, copy: bool) !*MalType {
        const mal = try allocator.create(MalType);
        errdefer allocator.destroy(mal);
        const data = if (copy) try allocator.dupe(u8, value) else value;
        mal.* = .{.Keyword=.{.data = data}};
        if (debug_alloc) warn("Init {any}", .{mal});
        return mal;
    }

    pub fn new_int(value: i64) !*MalType {
        const mal = try allocator.create(MalType);
        mal.* = .{.Int=.{.data = value}};
        if (debug_alloc) warn("Init {any}", .{mal});
        return mal;
    }

    pub fn new_bool(b: bool) *MalType {
        if(b) {
            return &TRUE;
        }
        else {
            return &FALSE;
        }
    }

    pub fn newFnCore(f: *const fn (args: []*MalType) MalError!*MalType) !*MalType {
        const mal = try allocator.create(MalType);
        mal.* = .{.FnCore=.{.data = f}};
        if (debug_alloc) warn("Init core function", .{});
        return mal;
    }

    pub fn newFunc(arg_list: *MalType,
                   body: *MalType,
                   environment: *Env,
                  ) !*MalType
    {
        const mal = try allocator.create(MalType);
        mal.* = .{.Func=.{
            .arg_list = arg_list,
            .body = body,
            .environment = environment,
        }};
        if (debug_alloc) warn("Init {any}", .{mal});
        return mal;
    }

    pub fn new_list() !*MalType {
        const mal = try allocator.create(MalType);
        mal.* = .{.List=.{.data = MalLinkedList { }}};
        if (debug_alloc) warn("Init {any}", .{mal});
        return mal;
    }

    pub fn new_vector() !*MalType {
        const mal = try allocator.create(MalType);
        errdefer allocator.destroy(mal);
        mal.* = .{.Vector=.{.data = MalLinkedList { }}};
        if (debug_alloc) warn("Init {any}", .{mal});
        return mal;
    }

    pub fn new_atom(mal: *MalType) !*MalType {
        const new_mal = try allocator.create(MalType);
        errdefer allocator.destroy(new_mal);
        new_mal.* = .{.Atom=.{.data = mal}};
        if (debug_alloc) warn("Init {any}", .{new_mal});
        return new_mal;
    }

    pub fn new_hashmap() !*MalType {
        const new_mal = try allocator.create(MalType);
        errdefer allocator.destroy(new_mal);
        new_mal.* = .{.HashMap=.{.data = .{}}};
        if (debug_alloc) warn("Init {any}", .{new_mal});
        return new_mal;
    }

    //  Trivial but convenient checkers/getters.

    pub fn as_slice(self: MalType) ![]*MalType {
        return switch (self) {
            .List, .Vector => |x| x.data.items,
            else => MalError.TypeError,
        };
    }

    pub fn as_int(mal: MalType) !i64 {
        return switch (mal) {
            .Int => |val| val.data,
            else => MalError.TypeError,
        };
    }

    pub fn as_string(self: MalType) ![]const u8 {
        return switch (self) {
            .String => |s| s.data,
            else => MalError.TypeError,
        };
    }

    pub fn as_map(self: MalType) !MalHashMap {
        switch (self) {
            .HashMap => |x| return x.data,
            else => return MalError.TypeError,
        }
    }

    pub fn decref(mal: *MalType) void {
        switch(mal.*) {
            .List, .Vector => |*l| {
                std.debug.assert (0 < l.reference_count);
                l.reference_count -= 1;
                if (l.reference_count == 0) {
                    if (debug_alloc) warn("Free {any}", .{mal});
                    linked_list.list_destroy(&l.data);
                    l.metadata.decref();
                    allocator.destroy(mal);
                }
            },
            .Keyword, .String, .Symbol => |*l| {
                std.debug.assert (0 < l.reference_count);
                l.reference_count -= 1;
                if (l.reference_count == 0) {
                    if (debug_alloc) warn("Free {s} {any}", .{l.data, mal});
                    allocator.free(l.data);
                    allocator.destroy(mal);
                }
            },
            .Atom => |*l| {
                std.debug.assert (0 < l.reference_count);
                l.reference_count -= 1;
                if (l.reference_count == 0) {
                    if (debug_alloc) warn("Free {any}", .{mal});
                    l.data.decref();
                    allocator.destroy(mal);
                }
            },
            .HashMap => |*l| {
                std.debug.assert (0 <= l.reference_count);
                l.reference_count -= 1;
                if (l.reference_count == 0) {
                    if (debug_alloc) warn("Free {any}", .{mal});
                    map_destroy(&l.data);
                    l.metadata.decref();
                    allocator.destroy(mal);
                }
            },
            .Func => |*l| {
                std.debug.assert (0 < l.reference_count);
                l.reference_count -= 1;
                if (l.reference_count == 0) {
                    if (debug_alloc) warn("Free {any}", .{mal});
                    l.arg_list.decref();
                    l.body.decref();
                    l.environment.decref();
                    l.metadata.decref();
                    allocator.destroy(mal);
                }
            },
            .Int => |*l| {
                std.debug.assert (0 < l.reference_count);
                l.reference_count -= 1;
                if (l.reference_count == 0) {
                    if (debug_alloc) warn("Free {any}", .{mal});
                    allocator.destroy(mal);
                }
            },
            .FnCore => |*l| {
                std.debug.assert (0 < l.reference_count);
                l.reference_count -= 1;
                if (l.reference_count == 0) {
                    if (debug_alloc) warn("Free {any}", .{mal});
                    l.metadata.decref();
                    allocator.destroy(mal);
                }
            },
            .Nil, .False, .True => {},
        }
    }

    pub fn incref(mal: *MalType) void {
        // A procedure instead of a function returning its argument
        // because it must most of the time be applied *after* a
        // successful assignment.
        switch(mal.*) {
            .List, .Vector             => |*l| l.reference_count += 1,
            .Int                       => |*l| l.reference_count += 1,
            .Keyword, .String, .Symbol => |*l| l.reference_count += 1,
            .FnCore                    => |*l| l.reference_count += 1,
            .Func                      => |*l| l.reference_count += 1,
            .Atom                      => |*l| l.reference_count += 1,
            .HashMap                   => |*l| l.reference_count += 1,
            .Nil, .False, .True        => {},
        }
    }

};
