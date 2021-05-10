const std = @import("std");
const Allocator = std.mem.Allocator;

pub const MalType = union(enum) {
    pub const Number = i32;
    pub const Symbol = []const u8;
    pub const Atom = union(enum) {
        symbol: Symbol,
        number: Number,
        // TODO: nil, true, false, string
        // TODO: keyword
    };
    pub const List = std.ArrayList(MalType);

    pub const TypeError = error{
        NotAtom,
        NotNumber,
        NotSymbol,
        NotList,
    };

    atom: Atom,
    list: List,

    pub fn initNumber(num: Number) MalType {
        return .{ .atom = .{ .number = num } };
    }

    pub fn initList(list: List) MalType {
        return .{ .list = list };
    }

    pub fn initListAlloc(allocator: *Allocator) !MalType {
        return MalType{ .list = try List.init(allocator) };
    }

    pub fn initListCapacity(allocator: *Allocator, num: usize) !MalType {
        return MalType{ .list = try List.initCapacity(allocator, num) };
    }

    const Self = @This();

    // pub fn listMap(self: Self, allocator: *Allocator, mapFn: fn (element: *const MalType) !MalType) !MalType {
    //     var results = List.initCapacity(allocator, list.items.len);
    //     var results_list = results.asList();
    //     for (self.asList()) |item| {
    //         const result = try mapFn(allocator, item, repl_env);
    //         try results_list.append(result);
    //     }
    //     return results;
    // }

    pub fn asAtom(self: Self) !Atom {
        return switch (self) {
            .atom => |atom| atom,
            else => error.NotAtom,
        };
    }

    pub fn asNumber(self: Self) !Number {
        return switch (self) {
            .atom => |atom| switch (atom) {
                .number => |number| number,
                else => error.NotNumber,
            },
            else => error.NotNumber,
        };
    }

    pub fn asSymbol(self: Self) !Symbol {
        return switch (self) {
            .atom => |atom| switch (atom) {
                .symbol => |symbol| symbol,
                else => error.NotSymbol,
            },
            else => error.NotSymbol,
        };
    }

    pub fn asList(self: Self) !List {
        return switch (self) {
            .list => |list| list,
            else => error.NotList,
        };
    }
};

pub const MalTypeEval = union(enum) {
    pub const List = std.ArrayList(MalTypeEval);
    pub const Function = union(enum) {
        op_2_number: fn (a: MalType.Number, b: MalType.Number) MalType.Number,
    };

    pub const EvalError = error{
        NotNumber,
        NotFunction,
    };

    mal_type: MalType,
    list: List,
    function: Function,

    pub fn initListCapacity(allocator: *Allocator, num: usize) !MalTypeEval {
        return MalTypeEval{ .list = try List.initCapacity(allocator, num) };
    }

    const Self = @This();

    pub fn asNumber(self: Self) !MalType.Number {
        return switch (self) {
            .mal_type => |mal_type| switch (mal_type) {
                .atom => |atom| switch (atom) {
                    .number => |number| number,
                    else => error.NotNumber,
                },
                else => error.NotNumber,
            },
            else => error.NotNumber,
        };
    }

    pub fn asFunction(self: Self) !Function {
        return switch (self) {
            .function => |function| function,
            else => error.NotFunction,
        };
    }
};
