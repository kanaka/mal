const io = @import("std").io;
const fmt = @import("std").fmt;
const warn = @import("std").debug.warn;
const mem = @import("std").mem;
const math = @import("std").math;

const Allocator = @import("std").heap.c_allocator;

const MalType = @import("types.zig").MalType;
const MalTypeValue = @import("types.zig").MalTypeValue;
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

fn appendToBuffer(resize_buffer: *ResizeBuffer, buffer: []const u8) MalError!void {
    const n: usize = buffer.len;
    
    if(n + resize_buffer.pos > resize_buffer.len or resize_buffer.buffer == null) {
        const new_len = math.max(math.max(2*resize_buffer.len, 10), n+resize_buffer.pos);
        var bigger_buffer: [] u8 = Allocator.alloc(u8, new_len) catch return MalError.SystemError;
        if(resize_buffer.buffer) |old_buffer| {
            var i: usize = 0;
            while(i < resize_buffer.len) {
                bigger_buffer[i] = old_buffer[i];
                i += 1;
            }
            Allocator.free(old_buffer);
        }
        resize_buffer.buffer = bigger_buffer;
        resize_buffer.len = new_len;
    }

    if(resize_buffer.buffer) |n_buffer| {
        var i: usize = 0;
        while(i < n) {
            n_buffer[resize_buffer.pos] = buffer[i];
            i += 1;
            resize_buffer.pos += 1;
        }
    }
}

fn print_mal_to_buffer(mal: *const MalType, readable: bool) MalError!ResizeBuffer {
    var rb = ResizeBuffer{
        .buffer = null,
        .pos = 0,
        .len = 0,
    };

    try print_to_buffer(mal, &rb, readable);
    return rb;
}

pub fn print_str(optional_mal: ?*const MalType) MalError![] const u8 {
    const stdout_file = io.getStdOut() catch return MalError.SystemError;
    if(optional_mal == null) {
        var return_string: [] u8 = Allocator.alloc(u8, 3) catch return MalError.SystemError;
        return_string[0] = 'E'; //TODO: memcpy
        return_string[1] = 'O';
        return_string[2] = 'F';
        return return_string; // TODO: is this right?
        //stdout_file.write("EOF\n") catch return MalError.SystemError;
    }
    const mal = optional_mal orelse return "";
    var rb = try print_mal_to_buffer(mal, true);

    if(rb.buffer) |buffer| {
        //stdout_file.write(buffer[0..rb.pos]) catch return MalError.SystemError;
        //stdout_file.write("\n") catch return MalError.SystemError;
        var return_string: [] u8 = Allocator.alloc(u8, rb.pos) catch return MalError.SystemError;
        var i: usize = 0; // TODO: replace with memcpy (and elsewhere)
        while(i < rb.pos) {
            return_string[i] = buffer[i];
            i += 1;
        }
        Allocator.free(buffer);
        return return_string;
    }
    return MalError.SystemError;
}

pub fn print_mal_to_string(args: MalLinkedList, readable: bool, sep: bool) MalError![] u8 {
    // TODO: handle empty string
    var rb = ResizeBuffer{
        .buffer = null,
        .pos = 0,
        .len = 0,
    };

    var iterator = args.iterator();
    var first: bool = true;
    while(iterator.next()) |node| {
        if(!first and sep) {
            try appendToBuffer(&rb, " ");
        }
        try print_to_buffer(node, &rb, readable);
        first = false;
    }

    // TODO: is this the right exception?
    if(rb.buffer) |buffer| {
        const len = rb.pos;
        var return_string: [] u8 = Allocator.alloc(u8, len) catch return MalError.SystemError;
        var i: usize = 0;
        while(i < len) {
            return_string[i] = buffer[i];
            i += 1;
        }
        Allocator.free(buffer);
        return return_string;
    }
    const s: []u8 = "";
    return s;
}

fn print_to_buffer(mal: *const MalType, rb: *ResizeBuffer, readable: bool) MalError!void {
    switch(mal.data) {
        .String => |string| {
            if(readable) {
                try appendToBuffer(rb, "\"");
            }
            // TODO: optimize this
            var i: usize = 0;
            var n: usize = string.len;
            while(i < n){
                const this_char = string[i];
                if(readable and (this_char == '"' or this_char==92)) {
                    try appendToBuffer(rb, backslash);
                }
                if(readable and (this_char == '\n')) {
                    try appendToBuffer(rb, "\\n");
                }
                else {
                    try appendToBuffer(rb, string[i..i+1]);
                }
                i += 1;
            }
            if(readable) {
                try appendToBuffer(rb, "\"");
            }
        },
        .Keyword => |kwd| {
            try appendToBuffer(rb, ":");
            try appendToBuffer(rb, kwd[1..kwd.len]);
        },
        .Int => |val| {
            try fmt.format(rb, MalError, appendToBuffer, "{0}", val);
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
            var iterator = l.iterator();
            var first_iteration = true;
            while(iterator.next()) |next_mal| {
                if(!first_iteration) {
                    try appendToBuffer(rb, " ");
                }
                try print_to_buffer(next_mal, rb, readable);
                first_iteration = false;
            }
            try appendToBuffer(rb, ")");
        },
        .Vector => |v| {
            try appendToBuffer(rb, "[");
            var iterator = v.iterator();
            var first_iteration = true;
            while(iterator.next()) |next_mal| {
                if(!first_iteration) {
                    try appendToBuffer(rb, " ");
                }
                try print_to_buffer(next_mal, rb, readable);
                first_iteration = false;
            }
            try appendToBuffer(rb, "]");
        },
        .Atom => |atom_value| {
            try appendToBuffer(rb, "(atom ");
            try print_to_buffer(atom_value.*, rb, readable);
            try appendToBuffer(rb, ")");
        },
        .Func, .Fn0, .Fn1, .Fn2, .Fn3, .Fn4, .FVar => {
            try appendToBuffer(rb, "#<function>");
        },
        .Generic => |value| {
            try appendToBuffer(rb, value);
        },
        .HashMap => |h| {
            try appendToBuffer(rb, "{");
            var iterator = h.iterator();
            var first = true;
            while(true) {
                const optional_pair = iterator.next();
                const pair = optional_pair orelse break;
                if(!first) {
                    try appendToBuffer(rb, " ");
                }                
                if(pair.key.len > 1 and pair.key[0] == 255) {
                    try appendToBuffer(rb, ":");
                    try appendToBuffer(rb, pair.key[1..pair.key.len]);
                }
                else {
                    try appendToBuffer(rb, "\"");
                    try appendToBuffer(rb, pair.key);
                    try appendToBuffer(rb, "\"");
                }
                try appendToBuffer(rb, " ");
                try print_to_buffer(pair.value, rb, readable);
                first = false;
            }
            try appendToBuffer(rb, "}");
        },
    }
}
