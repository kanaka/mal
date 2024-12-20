const fmt = @import("std").fmt;

const pcre = @cImport({
    @cInclude("pcre.h");
});

const MalType = @import("types.zig").MalType;
const MalError = @import("error.zig").MalError;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;

const Allocator = @import("std").heap.c_allocator;
const string_eql = @import("std").hash_map.eqlString;
const linked_list = @import("linked_list.zig");
const assert = @import("std").debug.assert;
const throw = @import("error.zig").throw;
const MalHashMap = @import("hmap.zig").MalHashMap;
const map_insert_incref_key = @import("hmap.zig").map_insert_incref_key;

const match: [*]const u8 =
    \\[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
;
var error_msg: [*c]const u8 = undefined;
var erroroffset: c_int = 0;
var re: ?*pcre.pcre = null;

const Reader = struct {
    position: u32,
    string: [] const u8,
    tokens: [] usize,

    pub fn init(string: [] const u8, tokens: [] usize) Reader {
        return Reader {
            .position = 0,
            .string = string,
            .tokens = tokens,
        };
    }

    pub fn next(self: *Reader) void {
        self.position += 1;
    }

    pub fn peek(self: *Reader) ?[]const u8 {
        while(!self.eol()) {
            const start = self.tokens[2*self.position];
            const end = self.tokens[2*self.position+1];
            if(self.string[start] == ';') {
                self.position += 1;
                continue;
            }
            return self.string[start..end];
        }
        return null;
    }

    pub fn eol(self: *Reader) bool {
        return (2 * self.position >= self.tokens.len);
    }
};

const AliasPair = struct {
    name: []const u8,
    value: []const u8,
    count: u8,
};

const alias_pairs = [_] AliasPair {
    AliasPair {.name="@", .value="deref", .count=1},
    AliasPair {.name="\'", .value="quote", .count=1},
    AliasPair {.name="`", .value="quasiquote", .count=1},
    AliasPair {.name="~", .value="unquote", .count=1},
    AliasPair {.name="~@", .value="splice-unquote", .count=1},
    AliasPair {.name="^", .value="with-meta", .count=2},
};

pub fn read_form(reader: *Reader) MalError!*MalType {
    const token = reader.peek() orelse return MalError.ArgError;
    reader.next();
    if(token[0] == '(') {
        return try read_list(reader);
    }
    else if(token[0] == '[') {
        return try read_vector(reader);
    }
    else if(token[0] == ':') {
        return MalType.new_keyword(token[1..], true);
    }
    else if(token[0] == '{') {
        return try read_hashmap(reader);
    }
    for(alias_pairs) |pair| {
        const name = pair.name;
        const value = pair.value;
        const count = pair.count;
        if(string_eql(token, name)) {
            assert (count == 1 or count == 2);
            const result = try MalType.new_list();
            errdefer result.decref();
            const first = try MalType.new_symbol(value, true);
            try result.List.data.append(Allocator, first);
            for(0..count) |_| {
                const second = try read_form(reader);
                errdefer second.decref();
                try result.List.data.insert(Allocator, 1, second);
            }
            return result;
        }
    }
    if(token_is_int(token)) {
        const value = try fmt.parseInt(i32, token, 10);
        return try MalType.new_int(value);
    }
    else if(string_eql(token, "nil")) {
        return &MalType.NIL;
    }
    else if(string_eql(token, "true")) {
        return &MalType.TRUE;
    }
    else if(string_eql(token, "false")) {
        return &MalType.FALSE;
    }
    else if(token[0] == '"') {
        return try read_atom_string(token);
    }
    else {
        return try MalType.new_symbol(token, true);
    }
}

fn read_list(reader: *Reader) !*MalType {
    const result = try MalType.new_list();
    errdefer result.decref();
    while(try read_list_element(reader, ')', "unbalanced '('")) |mal| {
        try result.List.data.append(Allocator, mal);
    }
    return result;
}

fn read_vector(reader: *Reader) !*MalType {
    const result = try MalType.new_vector();
    errdefer result.decref();
    while(try read_list_element(reader, ']', "unbalanced '['")) |mal| {
        try result.Vector.data.append(Allocator, mal);
    }
    return result;
}

fn read_hashmap(reader: *Reader) !*MalType {
    const result = try MalType.new_hashmap();
    errdefer result.decref();
    while(try read_list_element(reader, '}', "unbalanced '{'")) |key| {
        const value = try read_form(reader);
        errdefer value.decref();
        try map_insert_incref_key(&result.HashMap.data, key, value);
        key.decref();
    }
    return result;
}

fn read_list_element(reader: *Reader,
                     comptime closer: u8,
                     comptime unbalanced: []const u8,
                    ) !?*MalType {
    if(reader.peek()) |next_token| {
        if(next_token[0] == closer) {
            reader.next();
            return null;
        }
        return try read_form(reader);
    }
    return throw(try MalType.new_string(unbalanced, true));
}

fn char_is_int(c: u8) bool {
    return (c >= '0' and c <= '9');
}

fn token_is_int(token: []const u8) bool {
    if(char_is_int(token[0]))
        return true;
    if(token.len >= 2 and token[0] == '-' and char_is_int(token[1]))
        return true;
    return false;
}

fn read_atom_string(token: []const u8) MalError!*MalType {
    const n = token.len;
    if(token[0] != '"' or token[n-1] != '"' or n <= 1) {
        return throw(try MalType.new_string("unbalanced '\"'", true));
    }

    var tmp_buffer = try Allocator.alloc(u8, n-2);
    errdefer Allocator.free(tmp_buffer);
    var i: usize = 1;
    var j: usize = 0;
    const escape_char: u8 = '\\'; //TODO: remove this comment required by bad emacs config '
    while(i < n-1) {
        if(token[i] != escape_char) {
            tmp_buffer[j] = token[i];
            j += 1;
            i += 1;
        }
        else {
            if(i==n-2) {
                return throw(try MalType.new_string("unbalanced '\"'", true));
            }
            if(token[i+1] == 'n') {
                tmp_buffer[j] = '\n';
            } else {
                tmp_buffer[j] = token[i+1];
            }
            j += 1;
            i += 2;
        }
    }

    return try MalType.new_string(tmp_buffer[0..j], false);
}

pub fn read_str(string: [] const u8) MalError!Reader {
    if(re == null) {
        re = pcre.pcre_compile(&match[0], 0, &error_msg, &erroroffset, 0);
    }    
    const tokens = try tokenize(re, string);
    return Reader.init(string, tokens);
}

// Allocates an array of matches.  Caller is becomes owner of memory.
fn tokenize(regex: ?*pcre.pcre, string: [] const u8) MalError![] usize {
    // TODO: pass in allocator
    const buffer_size: usize = 3 * string.len + 10;
    var indices: [] c_int = try Allocator.alloc(c_int, buffer_size);
    defer Allocator.free(indices);
    var match_buffer: [] usize = try Allocator.alloc(usize, buffer_size);
    defer Allocator.free(match_buffer);
    var current_match: usize = 0;
    var start_pos: c_int = 0;
    
    var rc: c_int = 0;
    var start_match: usize = 0;
    var end_match: usize = 0;
    const subject_size: c_int = @intCast(string.len);

    while(start_pos < subject_size) {
        rc = pcre.pcre_exec(regex, 0, &string[0], subject_size, start_pos, 0,
                            &indices[0], @intCast(buffer_size));
        if(rc <= 0)
            break;
        start_pos = indices[1];
        start_match = @intCast(indices[2]);
        end_match = @intCast(indices[3]);
        match_buffer[current_match] = start_match;
        match_buffer[current_match+1] = end_match;
        current_match += 2;
    }

    var matches: [] usize = try Allocator.alloc(usize, current_match);
    for(0..current_match) |i| {
        matches[i] = match_buffer[i];
    }

    return matches;
}
