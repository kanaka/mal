const warn = @import("std").debug.warn;

const Allocator = @import("std").mem.Allocator;

pub fn string_eql(a: []const u8, b: []const u8) bool {
    if(a.len != b.len) {
        return false;
    }
    const n = a.len;
    var i: usize = 0;
    while(i < n) {
        if(a[i] != b[i]) {
            return false;
        }
        i += 1;
    }
    return true;
}

pub fn string_copy(allocator: *Allocator, str: []const u8) ![]const u8 {
    const copy = try allocator.alloc(u8, str.len);
    var i: usize = 0;
    while(i < str.len) {
        copy[i] = str[i];
        i += 1;
    }
    return copy;
}

pub fn string_concat(allocator: *Allocator, s1: []const u8, s2: []const u8) ![] const u8 {
    const n: usize = s1.len + s2.len;
    var i: usize = 0;
    var pos: usize = 0;
    const copy = try allocator.alloc(u8, n);
    while(i < s1.len) {
        copy[pos] = s1[i];
        pos += 1;
        i += 1;
    }
    i = 0;
    while(i < s2.len) {
        copy[pos] = s2[i];
        pos += 1;
        i += 1;
    }
    return copy;
}

