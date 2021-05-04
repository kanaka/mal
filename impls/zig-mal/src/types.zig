const std = @import("std");

pub const MalType = union(enum) {
    Atom: union(enum) {
        Symbol: []const u8,
        Number: i32,
        // TODO: nil, true, false, string
        // TODO: keyword
    },
    List: std.ArrayList(MalType),
};
