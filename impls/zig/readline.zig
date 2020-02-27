const Allocator = @import("std").mem.Allocator;
const readline = @cImport(
    @cInclude("readline/readline.h"));
const rl_hist = @cImport(
    @cInclude("readline/history.h"));
const free = @import("std").c.free;
const addNullByte = @import("std").cstr.addNullByte;

const warn = @import("std").debug.warn;

pub fn slice_from_cstr(allocator: *Allocator, str: [*]const u8) ![]u8{
    var length: usize = 0;
    while(true) {
        if(str[length] == 0)
            break;
        length += 1; 
    }
    // TODO: check for 0-length
    const slice = try allocator.alloc(u8, length);
    var i: usize = 0;
    while(i < length) {
        slice[i] = str[i];
        i += 1;
    }
    return slice;
}

pub fn getline(allocator: *Allocator) !?[] u8 {
    var input: ?[*] u8 = readline.readline(c"user> ");
    if(input) |actual| {
        const aslice = try slice_from_cstr(allocator, actual);
        rl_hist.add_history(actual);
        free(actual);
        return aslice;
    }
    return null;
}

pub fn getline_prompt(allocator: *Allocator, prompt: []const u8) !?[] u8 {
    const null_terminated_prompt = try addNullByte(allocator, prompt);
    var input: ?[*] u8 = readline.readline(&null_terminated_prompt[0]);
    allocator.free(null_terminated_prompt);
    if(input) |actual| {
        const aslice = try slice_from_cstr(allocator, actual);
        rl_hist.add_history(actual);
        free(actual);
        return aslice;
    }
    return null;
}
