const std = @import("std");
const warn = @import("std").debug.warn;

const reader = @import("reader.zig");
const pcre = reader.pcre;
const printer = @import("printer.zig");
const getline = @import("readline.zig").getline;

const Allocator = @import("std").heap.c_allocator;

const MalType = @import("types.zig").MalType;

fn READ(a: []const u8) !?*MalType {
    var read = try reader.read_str(a);
    var optional_mal = reader.read_form(&read);
    return optional_mal;
}

fn EVAL(a: ?*MalType) ?*MalType {
    return a;
}

fn PRINT(optional_mal: ?*MalType) ![]const u8 {
    return printer.print_str(optional_mal);
}

fn rep(input: []const u8) ![]const u8 {
    var read_input = READ(input) catch null;
    var eval_input = EVAL(read_input);
    var print_input = PRINT(eval_input);
    if(eval_input) |mal| {
        mal.delete(Allocator);
    }
    return print_input;
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut();
    while(true) {
        var line = (try getline(Allocator)) orelse break;
        var output = try rep(line);
        try stdout_file.writeAll(output);
        Allocator.free(output);
        try stdout_file.writeAll("\n");
    }
}
