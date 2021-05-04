const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("./types.zig");
const MalType = types.MalType;

const Error = error{OutOfMemory};

pub fn pr_str(allocator: *Allocator, form: *const MalType) Error![]const u8 {
    // TODO: this needs a significant refactoring to work with an allocator
    // other than arena, not planned for deallocation
    return switch (form.*) {
        .Atom => |atom| switch (atom) {
            .Symbol => |symbol| symbol,
            .Number => |number| try std.fmt.allocPrint(allocator, "{d}", .{number}),
        },
        .List => |list| {
            var printed_forms = std.ArrayList(u8).init(allocator);
            const writer = printed_forms.writer();

            try writer.writeAll("(");
            for (list.items) |list_form, index| {
                const printed_form = try pr_str(allocator, &list_form);
                try writer.writeAll(printed_form);
                if (index < list.items.len - 1) {
                    try writer.writeAll(" ");
                }
            }
            try writer.writeAll(")");

            return printed_forms.items;
        },
    };
}
