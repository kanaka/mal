const std = @import("std");
const Allocator = std.mem.Allocator;

const printer = @import("./printer.zig");
const reader = @import("./reader.zig");
const types = @import("./types.zig");
const MalType = types.MalType;

const input_buffer_length = 256;
const prompt = "user> ";

fn READ(allocator: Allocator, input: []const u8) !MalType {
    const form = try reader.read_str(allocator, input);
    return form;
}

fn EVAL(allocator: Allocator, form: *const MalType) !MalType {
    // const result = form;
    var result = try allocator.create(MalType);
    result.* = form.*;
    return result.*;
}

fn PRINT(allocator: Allocator, form: *const MalType) ![]const u8 {
    const output = try printer.pr_str(allocator, form);
    return output;
}

fn rep(allocator: Allocator, input: []const u8) ![]const u8 {
    const form = try READ(allocator, input);
    const result = try EVAL(allocator, &form);
    const output = try PRINT(allocator, &result);
    return output;
}

pub fn main() anyerror!void {
    // general purpose allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var input_buffer: [input_buffer_length]u8 = undefined;
    // initialize std io reader and writer
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
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
        // arena allocator, memory is freed at end of loop iteration
        var arena = std.heap.ArenaAllocator.init(gpa.allocator());
        defer arena.deinit();

        // read-eval-print
        if (rep(arena.allocator(), line)) |result|
            try stdout.print("{s}\n", .{result})
        else |err| {
            const message = switch (err) {
                error.EndOfInput => "unexpected end of input",
                error.ListNoClosingTag => "unbalanced list form, missing closing ')'",
                error.StringLiteralNoClosingTag => "unbalanced string literal, missing closing '\"'",
                error.TokensPastFormEnd => "found additional tokens past end of form",
                error.OutOfMemory => "out of memory",
            };
            try stderr.print("Error: {s}\n", .{message});
        }
    }
}
