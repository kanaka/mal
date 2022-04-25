const std = @import("std");

const input_buffer_length = 256;
const prompt = "user> ";

fn READ(input: []u8) []u8 {
    const output = input;
    return output;
}

fn EVAL(input: []u8) []u8 {
    const output = input;
    return output;
}

fn PRINT(input: []u8) []u8 {
    const output = input;
    return output;
}

fn rep(input: []u8) []u8 {
    const output = PRINT(EVAL(READ(input)));
    return output;
}

pub fn main() anyerror!void {
    var input_buffer: [input_buffer_length]u8 = undefined;
    // initialize std io reader and writer
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    // main repl loop
    while (true) {
        // print prompt
        // TODO: line editing and history, (readline, editline, linenoise)
        try stdout.print(prompt, .{});
        // read line of input
        const line = (try stdin.readUntilDelimiterOrEof(&input_buffer, '\n')) orelse {
            // reached input end-of-file
            break;
        };
        // read-eval-print
        const result = rep(line);
        try stdout.print("{s}\n", .{result});
    }
}
