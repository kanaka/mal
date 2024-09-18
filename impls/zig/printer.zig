const io = @import("std").io;
const fmt = @import("std").fmt;

const Allocator = @import("std").heap.c_allocator;

const MalType = @import("types.zig").MalType;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const MalError = @import("error.zig").MalError;

const ResizeBuffer = struct {
    buffer: ?[]u8,
    pos: usize,
    len: usize,
};

// TODO fix emacs highlighting, remove this
const backslash =
    \\\
;

fn appendToBuffer(resize_buffer: *ResizeBuffer, buffer: []const u8) !void {
    const n: usize = buffer.len;
    
    if(n + resize_buffer.pos > resize_buffer.len or resize_buffer.buffer == null) {
        var new_len: usize = 10;
        const new_len2 = 2*resize_buffer.len;
        if(new_len < new_len2)
            new_len = new_len2;
        const new_len3 = n+resize_buffer.pos;
        if(new_len < new_len3)
            new_len = new_len3;
        var bigger_buffer: [] u8 = try Allocator.alloc(u8, new_len);
        if(resize_buffer.buffer) |old_buffer| {
            for(0..resize_buffer.len) |i|
                bigger_buffer[i] = old_buffer[i];
            Allocator.free(old_buffer);
        }
        resize_buffer.buffer = bigger_buffer;
        resize_buffer.len = new_len;
    }

    if(resize_buffer.buffer) |n_buffer|
        for(0..n) |i| {
            n_buffer[resize_buffer.pos] = buffer[i];
            resize_buffer.pos += 1;
        };
}

//  TODO: Writer and ResizeBuffer should probably me merged.
fn writeFn(context: *ResizeBuffer, bytes: []const u8) !usize {
    try appendToBuffer(context, bytes);
    return bytes.len;
}
pub const Writer = io.Writer(*ResizeBuffer, MalError, writeFn);
pub fn writer(rb: *ResizeBuffer) Writer {
    return .{ .context = rb };
}

pub fn print_str(mal: MalType) ![]const u8 {
    //  const stdout_file = io.getStdOut();

    var rb = ResizeBuffer{
        .buffer = null,
        .pos = 0,
        .len = 0,
    };
    try print_to_buffer(mal, &rb, true);
    if(rb.buffer) |buffer| {
        //stdout_file.write(buffer[0..rb.pos]);
        //stdout_file.write("\n");
        var return_string: [] u8 = try Allocator.alloc(u8, rb.pos);
        // TODO: replace with memcpy (and elsewhere)
        for (0..rb.pos) |i| {
            return_string[i] = buffer[i];
        }
        Allocator.free(buffer);
        return return_string;
    }
    return MalError.SystemError;
}

pub fn print_mal_to_string(args: []const *MalType, readable: bool, sep: bool) ![] u8 {
    // TODO: handle empty string
    var rb = ResizeBuffer{
        .buffer = null,
        .pos = 0,
        .len = 0,
    };

    for (args, 0..) |node, idx| {
        if(0 < idx and sep) {
            try appendToBuffer(&rb, " ");
        }
        try print_to_buffer(node.*, &rb, readable);
    }

    // TODO: is this the right exception?
    if(rb.buffer) |buffer| {
        const len = rb.pos;
        var return_string: [] u8 = try Allocator.alloc(u8, len);
        for (0..len) |i| {
            return_string[i] = buffer[i];
        }
        Allocator.free(buffer);
        return return_string;
    }
    const s: []u8 = "";
    return s;
}

fn print_to_buffer(mal: MalType, rb: *ResizeBuffer, readable: bool) !void {
    switch(mal) {
        .String => |string| {
            if(readable) {
                try appendToBuffer(rb, "\"");
                // TODO: optimize this
                for(string.data, 0..) |this_char, i| {
                  if(this_char == '"' or this_char==92) {
                    try appendToBuffer(rb, backslash);
                  }
                  if(this_char == '\n') {
                    try appendToBuffer(rb, "\\n");
                  }
                  else {
                    try appendToBuffer(rb, string.data[i..i+1]);
                  }
                }
                try appendToBuffer(rb, "\"");
            }
            else {
                try appendToBuffer(rb, string.data);
            }
        },
        .Keyword => |kwd| {
            try appendToBuffer(rb, ":");
            try appendToBuffer(rb, kwd.data);
        },
        .Int => |val| {
            try fmt.format(writer(rb), "{0}", .{val.data});
        },
        .Nil => {
            try appendToBuffer(rb, "nil");
        },
        .True => {
            try appendToBuffer(rb, "true");
        },
        .False => {
            try appendToBuffer(rb, "false");
        },
        .List => |l| {
            try appendToBuffer(rb, "(");
            for (l.data.items, 0..) |next_mal, i| {
                if(0<i) {
                    try appendToBuffer(rb, " ");
                }
                try print_to_buffer(next_mal.*, rb, readable);
            }
            try appendToBuffer(rb, ")");
        },
        .Vector => |v| {
            try appendToBuffer(rb, "[");
            for (v.data.items, 0..) |next_mal, i| {
                if(0<i) {
                    try appendToBuffer(rb, " ");
                }
                try print_to_buffer(next_mal.*, rb, readable);
            }
            try appendToBuffer(rb, "]");
        },
        .Atom => |atom_value| {
            try appendToBuffer(rb, "(atom ");
            try print_to_buffer(atom_value.data.*, rb, readable);
            try appendToBuffer(rb, ")");
        },
        .Func, .FnCore => {
            try appendToBuffer(rb, "#<function>");
        },
        .Symbol => |value| {
            try appendToBuffer(rb, value.data);
        },
        .HashMap => |h| {
            try appendToBuffer(rb, "{");
            var iterator = h.data.iterator();
            var first = true;
            while(iterator.next()) |pair| {
                if(!first) {
                    try appendToBuffer(rb, " ");
                }                
                switch (pair.key_ptr.*.*) {
                  .Keyword => |k| {
                    try appendToBuffer(rb, ":");
                    try appendToBuffer(rb, k.data);
                  },
                  .String => |s| {
                    try appendToBuffer(rb, "\"");
                    try appendToBuffer(rb, s.data);
                    try appendToBuffer(rb, "\"");
                  },
                  else => unreachable,
                }
                try appendToBuffer(rb, " ");
                try print_to_buffer(pair.value_ptr.*.*, rb, readable);
                first = false;
            }
            try appendToBuffer(rb, "}");
        },
    }
}
