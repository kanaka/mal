const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

/// Perform a replacement on an allocated buffer of pre-determined size. Caller must free returned memory.
// needles should be the same size as the replacements, 1-1 correspondence
// pub fn replaceMultipleOwned(comptime T: type, allocator: Allocator, input: []const T, needles: []const []const T, replacements: []const []const T) Allocator.Error![]T {
// pub fn replaceMultipleOwned(comptime T: type, allocator: Allocator, input: []const T, needles: anytype, replacements: anytype) Allocator.Error![]T {
pub fn replaceMultipleOwned(comptime T: type, comptime n: usize, allocator: Allocator, input: []const T, needles: [n][]const T, replacements: [n][]const T) Allocator.Error![]T {
    // TODO: support slices, other types using anytype,
    // assert ( @typeInfo(@TypeOf(needles)).Struct.fields.len
    // == @typeInfo(@TypeOf(replacements)).Struct.fields.len )

    // Empty needles will loop forever.
    for (needles) |needle| assert(needle.len > 0);

    // Calculate the size needed in an output buffer to perform a replacement.
    // The needle must not be empty.
    var replacementSize = blk: {
        var i: usize = 0;
        var size: usize = input.len;
        while (i < input.len) {
            for (needles) |needle, needle_index| {
                if (mem.indexOf(T, input[i..], needle) == @as(usize, 0)) {
                    size = size - needle.len + replacements[needle_index].len;
                    i += needle.len;
                    break; // for loop
                }
            } else {
                i += 1;
            }
        }

        break :blk size;
    };

    var output = try allocator.alloc(T, replacementSize);

    // Replace needle with replacement as many times as possible, writing to an output buffer which is assumed to be of
    // appropriate size. Use replacementSize to calculate an appropriate buffer size.
    // The needle must not be empty.
    var i: usize = 0;
    var slide: usize = 0;
    var num_replacements: usize = 0;
    while (slide < input.len) {
        for (needles) |needle, needle_index| {
            if (mem.indexOf(T, input[slide..], needle) == @as(usize, 0)) {
                const replacement = replacements[needle_index];
                mem.copy(T, output[i .. i + replacement.len], replacement);
                i += replacement.len;
                slide += needle.len;
                num_replacements += 1;
                break; // for loop
            }
        } else {
            output[i] = input[slide];
            i += 1;
            slide += 1;
        }
    }
    // return num_replacements;

    return output;
}
