const getline = @import("readline.zig").getline;

const Allocator = @import("std").heap.c_allocator;
const stdout_file = @import("std").io.getStdOut();

fn READ(a: []const u8) []const u8 {
    return a;
}

fn EVAL(a: []const u8) []const u8 {
    return a;
}

fn PRINT(a: []const u8) !void {
    try stdout_file.writeAll(a);
    try stdout_file.writeAll("\n");
}

fn rep(input: []const u8) !void {
    const read_input = READ(input);
    const eval_input = EVAL(read_input);
    try PRINT(eval_input);
}

pub fn main() !void {
    while(try getline("user> ")) |line| {
        defer Allocator.free(line);
        try rep(line);
    }
}
