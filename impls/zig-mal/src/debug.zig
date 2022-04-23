const std = @import("std");
const Allocator = std.mem.Allocator;

const Env = @import("./env.zig").Env;
const printer = @import("./printer.zig");
const MalType = @import("./types.zig").MalType;
const MalValue = @import("./types.zig").MalValue;

pub fn println(str: []const u8) void {
    std.debug.print("{s}\n", .{str});
}

pub fn print_ptr(str: []const u8, ptr: anytype) void {
    std.debug.print("{s} {*}\n", .{ str, ptr });
}

pub fn print_ast(a: Allocator, ast: *const MalType) void {
    std.debug.print("ast: {s}\n", .{printer.pr_str(a, &MalValue{ .mal_type = ast.* }, true)});
}

pub fn print_env(a: Allocator, env: Env) void {
    print_ptr("env: ", &env);

    var env_it = env.data.iterator();
    while (env_it.next()) |entry| {
        std.debug.print("  '{s}: {s}\n", .{ entry.key, printer.pr_str(a, &entry.value, true) });
    }
    if (env.outer) |outer| {
        print_ptr("outer: ", env.outer.?);
        var outer_env_it = outer.data.iterator();
        while (outer_env_it.next()) |entry| {
            std.debug.print("  '{s}: {s}\n", .{ entry.key, printer.pr_str(a, &entry.value, true) });
        }
    } else println("outer: null");
    println("");
}
