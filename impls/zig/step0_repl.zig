const std = @import("std");
const warn = @import("std").debug.warn;

const getline = @import("readline.zig").getline;

const Allocator = @import("std").heap.c_allocator;

fn READ(a: [] u8) [] u8 {
    return a;
}

fn EVAL(a: [] u8) [] u8 {
    return a;
}

fn PRINT(a: [] u8) [] u8 {
    return a;
}

fn rep(input: [] u8) [] u8 {
    var read_input = READ(input);
    var eval_input = EVAL(read_input);
    var print_input = PRINT(eval_input);
    return print_input;
}

pub fn main() !void {
    const stdout_file = try std.io.getStdOut();
    while(true) {
        var line = (try getline(Allocator)) orelse break;
        var output = rep(line);
        try stdout_file.write(output);
        Allocator.free(output);
        try stdout_file.write("\n");
    }
}
