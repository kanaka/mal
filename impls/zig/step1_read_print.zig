const reader = @import("reader.zig");
const printer = @import("printer.zig");
const getline = @import("readline.zig").getline;

const Allocator = @import("std").heap.c_allocator;

const MalType = @import("types.zig").MalType;
const get_error_data = @import("error.zig").get_error_data;
const stdout_file = @import("std").io.getStdOut();

fn READ(a: []const u8) !*MalType {
    var read = try reader.read_str(a);
    return reader.read_form(&read);
}

fn EVAL(a: *MalType) *MalType {
    a.incref();
    return a;
}

fn PRINT(mal: MalType) !void {
    try printer.one_stdout(mal);
    try stdout_file.writeAll("\n");
}

fn rep(input: []const u8) !void {
    const read_input = try READ(input);
    defer read_input.decref();
    const eval_input = EVAL(read_input);
    defer eval_input.decref();
    try PRINT(eval_input.*);
}

pub fn main() !void {
    while(try getline("user> ")) |line| {
        defer Allocator.free(line);
        rep(line) catch |err| {
            try stdout_file.writeAll("Error: ");
            try stdout_file.writeAll(@errorName(err));
            try stdout_file.writeAll("\n");
            if(get_error_data()) |mal| {
                defer mal.decref();
                try stdout_file.writeAll("MAL error object is: ");
                try PRINT(mal.*);
            }
        };
    }
}
