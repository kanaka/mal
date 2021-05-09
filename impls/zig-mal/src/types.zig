const std = @import("std");

const Number = i32;
const Symbol = []const u8;

const Atom = union(enum) {
    symbol: Symbol,
    number: Number,
    // TODO: nil, true, false, string
    // TODO: keyword
};

const List = std.ArrayList(MalType);

pub const MalType = union(enum) {
    const Self = @This();

    atom: Atom,
    list: List,

    pub fn asAtom(self: Self) Atom {
        return switch (self) {
            .atom => |atom| atom,
            else => unreachable,
        };
    }

    pub fn asNumber(self: Self) Number {
        return switch (self) {
            .atom => |atom| switch (atom) {
                .number => |number| number,
                else => unreachable,
            },
            else => unreachable,
        };
    }

    pub fn asSymbol(self: Self) Symbol {
        return switch (self) {
            .atom => |atom| switch (atom) {
                .symbol => |symbol| symbol,
                else => unreachable,
            },
            else => unreachable,
        };
    }
};
