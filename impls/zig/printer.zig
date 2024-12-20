const std = @import("std");
const stdout_writer = std.io.getStdOut().writer();
const Allocator = @import("std").heap.c_allocator;

const MalType = @import("types.zig").MalType;
const MalError = @import("error.zig").MalError;

// TODO fix emacs highlighting, remove this
const backslash =
    \\\
;

pub fn one_stdout(mal: MalType) !void {
    try print_to_buffer(mal, stdout_writer, true);
}

pub fn n_stdout(args: []const *MalType, readably: bool, sep: bool) !void {
    try n_writer(stdout_writer, args, readably, sep);
}

fn n_writer(rb: anytype, args: []const *MalType, readable: bool, sep: bool) !void {
    for (args, 0..) |node, idx| {
        if(0 < idx and sep) {
            try rb.writeAll(" ");
        }
        try print_to_buffer(node.*, rb, readable);
    }
}

pub fn print_mal_to_string(args: []const *MalType, readable: bool, sep: bool) ![]u8 {
    var rb = std.ArrayListUnmanaged(u8) { };
    errdefer rb.deinit(Allocator);
    const writer = rb.writer(Allocator);
    try n_writer(writer, args, readable, sep);
    return rb.toOwnedSlice(Allocator);
}

fn print_to_buffer(mal: MalType, rb: anytype, readable: bool) MalError!void {
    switch(mal) {
        .String => |string| {
            if(readable) {
                try rb.writeAll("\"");
                // TODO: optimize this
                for(string.data, 0..) |this_char, i| {
                  if(this_char == '"' or this_char==92) {
                    try rb.writeAll(backslash);
                  }
                  if(this_char == '\n') {
                    try rb.writeAll("\\n");
                  }
                  else {
                    try rb.writeAll(string.data[i..i+1]);
                  }
                }
                try rb.writeAll("\"");
            }
            else {
                try rb.writeAll(string.data);
            }
        },
        .Keyword => |kwd| {
            try rb.writeAll(":");
            try rb.writeAll(kwd.data);
        },
        .Int => |val| {
            try rb.print("{0}", .{val.data});
        },
        .Nil => {
            try rb.writeAll("nil");
        },
        .True => {
            try rb.writeAll("true");
        },
        .False => {
            try rb.writeAll("false");
        },
        .List => |l| {
            try rb.writeAll("(");
            try n_writer(rb, l.data.items, readable, true);
            try rb.writeAll(")");
        },
        .Vector => |v| {
            try rb.writeAll("[");
            try n_writer(rb, v.data.items, readable, true);
            try rb.writeAll("]");
        },
        .Atom => |atom_value| {
            try rb.writeAll("(atom ");
            try print_to_buffer(atom_value.data.*, rb, readable);
            try rb.writeAll(")");
        },
        .Func, .FnCore => {
            try rb.writeAll("#<function>");
        },
        .Symbol => |value| {
            try rb.writeAll(value.data);
        },
        .HashMap => |h| {
            try rb.writeAll("{");
            var iterator = h.data.iterator();
            var first = true;
            while(iterator.next()) |pair| {
                if(!first) {
                    try rb.writeAll(" ");
                }
                try print_to_buffer(pair.key_ptr.*.*, rb, true);
                try rb.writeAll(" ");
                try print_to_buffer(pair.value_ptr.*.*, rb, readable);
                first = false;
            }
            try rb.writeAll("}");
        },
    }
}
