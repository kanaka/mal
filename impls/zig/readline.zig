const allocator = @import("std").heap.c_allocator;
const readline = @cImport(
    @cInclude("readline/readline.h"));
const rl_hist = @cImport(
    @cInclude("readline/history.h"));
const free = @import("std").c.free;

fn addNullByte(prompt: []const u8) ![]u8 {
    const result = try allocator.alloc(u8, prompt.len + 1);
    for (0.., prompt) |i, source|
        result[i] = source;
    result[prompt.len] = 0;
    return result;
}

fn slice_from_cstr(str: [*]const u8) ![]const u8 {
    var length: usize = 0;
    while(str[length] != 0) {
        length += 1;
    }
    // TODO: check for 0-length
    const slice = try allocator.alloc(u8, length);
    for (str, 0..length) |source, i| {
        slice[i] = source;
    }
    return slice;
}

pub fn getline(prompt: []const u8) !?[]const u8 {
    const null_terminated_prompt = try addNullByte(prompt);
    defer allocator.free(null_terminated_prompt);
    const input = readline.readline(&null_terminated_prompt[0]);
    if(input) |actual| {
        defer free(actual);
        const aslice = try slice_from_cstr(actual);
        rl_hist.add_history(actual);
        return aslice;
    }
    return null;
}
