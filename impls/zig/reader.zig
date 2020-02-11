const fmt = @import("std").fmt;
const warn = @import("std").debug.warn;

pub const pcre = @cImport({
    @cInclude("pcre.h");
});

const MalType = @import("types.zig").MalType;
const MalData = @import("types.zig").MalData;
const MalTypeValue = @import("types.zig").MalTypeValue;
const MalError = @import("error.zig").MalError;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const printer = @import("printer.zig");

const Allocator = @import("std").heap.c_allocator;
const string_eql = @import("utils.zig").string_eql;
const linked_list = @import("linked_list.zig");

const match: [*]const u8 =
    c\\[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
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

    pub fn next(self: *Reader) []const u8 {
        const this_token = self.peek();
        self.position += 1;
        return this_token;
    }

    pub fn peek(self: *Reader) []const u8 {
        while(!self.eol()) {
            const start = self.tokens[2*self.position];
            const end = self.tokens[2*self.position+1];
            if(self.string[start] == ';') {
                self.position += 1;
                continue;
            }
            return self.string[start..end];
        }
        return "";
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

pub fn read_form(reader: *Reader) MalError!?*MalType {
    if(reader.eol()) {
        return null;
    }
    const token = reader.peek();
    if(token.len == 0) {
        return MalType.new_nil(Allocator);
    }
    if(token[0] == '(') {
        return try read_list(reader);
    }
    else if(token[0] == '[') {
        return try read_vector(reader);
    }
    else if(token[0] == ':') {
        const keyword = reader.next();
        return MalType.new_keyword(Allocator, keyword[1..keyword.len]);
    }
    else if(token[0] == '{') {
        return try read_hashmap(reader);
    }

    for(alias_pairs) |pair| {
        const name = pair.name;
        const value = pair.value;
        const count = pair.count;
        if(!string_eql(token, name)) {
            continue;
        }
        var new_ll = MalLinkedList.init(Allocator);
        const new_generic = try MalType.new_generic(Allocator, value);
        const tmp = reader.next();
        var num_read: u8 = 0;
        while(num_read < count) {
            const next_read = (try read_form(reader)) orelse return MalError.ArgError;
            try linked_list.prepend_mal(Allocator, &new_ll, next_read);
            num_read += 1;
        }
        try linked_list.prepend_mal(Allocator, &new_ll, new_generic);
        const new_list = try MalType.new_nil(Allocator);
        new_list.data = MalData {.List = new_ll};
        return new_list;
    }

    return try read_atom(reader);
}

pub fn read_list(reader: *Reader) MalError!*MalType {
    const first_token = reader.next();
    var new_ll = MalLinkedList.init(Allocator);
    const mal_list: *MalType = try MalType.new_nil(Allocator);
    
    while(!reader.eol()) {
        var next_token = reader.peek();

        if(next_token.len == 0) {
            return MalError.ReaderUnmatchedParen;
        }
        if(next_token[0] == ')') {
            const right_paren = reader.next();
            mal_list.data = MalData{.List = new_ll};
            return mal_list;
        }
        const mal = (try read_form(reader)) orelse return MalError.ArgError;
        try linked_list.append_mal(Allocator, &new_ll, mal);
    }
    return MalError.ReaderUnmatchedParen;
}

pub fn read_vector(reader: *Reader) MalError!*MalType {
    const first_token = reader.next();
    var new_ll = MalLinkedList.init(Allocator);
    const mal_list: *MalType = try MalType.new_nil(Allocator);
    
    while(!reader.eol()) {
        var next_token = reader.peek();

        if(next_token.len == 0) {
            return MalError.ReaderUnmatchedParen;
        }
        if(next_token[0] == ']') {
            const right_paren = reader.next();
            mal_list.data = MalData{.Vector = new_ll};
            return mal_list;
        }
        const mal = (try read_form(reader)) orelse return MalError.ArgError;
        try linked_list.append_mal(Allocator, &new_ll, mal);
    }
    return MalError.ReaderUnmatchedParen;
}


pub fn read_hashmap(reader: *Reader) MalError!*MalType {
    const first_token = reader.next();
    const new_hashmap = try MalType.new_hashmap(Allocator);
    while(!reader.eol()) {
        var next_token = reader.peek();

        if(next_token.len == 0) {
            return MalError.ReaderUnmatchedParen;
        }
        if(next_token[0] == '}') {
            const right_paren = reader.next();
            return new_hashmap;
        }
        const mal = (try read_form(reader)) orelse return MalError.ArgError;
        const key = switch(mal.data) {
            .String => |s| s,
            .Keyword => |kwd| kwd,
            else => return MalError.TypeError,
        };
        if(next_token.len == 0 or next_token[0] == '}') {
            return MalError.ReaderBadHashmap;
        }
        const val = (try read_form(reader)) orelse return MalError.ArgError;
        try new_hashmap.hashmap_insert(key, val);
    }
    return MalError.ReaderUnmatchedParen;
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

pub fn read_atom(reader: *Reader) MalError!*MalType {
    const token = reader.next();
    
    if(token_is_int(token)) {
        var mal_atom = try MalType.new_nil(Allocator);
        try read_atom_int(mal_atom, token);
        return mal_atom;
    }
    else if(string_eql(token, "nil")) {
        return MalType.new_nil(Allocator);
    }
    else if(string_eql(token, "true")) {
        return MalType.new_bool(Allocator, true);
    }
    else if(string_eql(token, "false")) {
        return MalType.new_bool(Allocator, false);
    }
    else if(token[0] == '"') {
        var mal_atom = try MalType.new_nil(Allocator);
        try read_atom_string(mal_atom, token);
        return mal_atom;
    }
    else {
        var mal_atom = try MalType.new_generic(Allocator, token);
        return mal_atom;
    }
}

fn read_atom_int(mal_atom: *MalType, token: []const u8) MalError!void {
    // TODO: extract int type from union
    mal_atom.data = MalData {.Int = fmt.parseInt(i32, token, 10)
                                 catch |err| return MalError.SystemError };
}

fn read_atom_string(mal_atom: *MalType, token: []const u8) MalError!void {
    const n = token.len;
    if(token[0] != '"' or token[n-1] != '"' or n <= 1) {
        return MalError.ReaderUnmatchedString;
    }

    if(n <= 2) {
        // We get here when the token is an empty string.
        // We encode this as MalTypeValue.String, with null .string_value
        var string = Allocator.alloc(u8, 0) catch return MalError.SystemError;
        mal_atom.data = MalData {.String = string};
        return;
    }
    
    var tmp_buffer = Allocator.alloc(u8, n-2) catch return MalError.SystemError;
    defer Allocator.free(tmp_buffer);
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
                return MalError.ReaderUnmatchedString;
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

    var string = Allocator.alloc(u8, j) catch return MalError.SystemError;
    i = 0;
    while(i < j) {
        string[i] = tmp_buffer[i];
        i += 1;
    }

    mal_atom.data = MalData {.String = string};
}

pub fn read_str(string: [] const u8) MalError!Reader {
    if(re == null) {
        re = pcre.pcre_compile(&match[0], 0, &error_msg, &erroroffset, 0);
    }    
    const tokens = try tokenize(re, string);
    return Reader.init(string, tokens);
}

// Allocates an array of matches.  Caller is becomes owner of memory.
pub fn tokenize(regex: ?*pcre.pcre, string: [] const u8) MalError![] usize {
    // TODO: pass in allocator
    const buffer_size: usize = 3 * string.len + 10;
    var indices: [] c_int = Allocator.alloc(c_int, buffer_size)
        catch return MalError.SystemError;
    defer Allocator.free(indices);
    var match_buffer: [] usize = Allocator.alloc(usize, buffer_size)
        catch return MalError.SystemError;
    defer Allocator.free(match_buffer);
    var current_match: usize = 0;
    var start_pos: c_int = 0;
    
    var rc: c_int = 0;
    var start_match: usize = 0;
    var end_match: usize = 0;
    const subject_size: c_int = @intCast(c_int, string.len);

    while(start_pos < subject_size) {
        rc = pcre.pcre_exec(regex, 0, &string[0], subject_size, start_pos, 0,
                            &indices[0], @intCast(c_int,buffer_size));
        if(rc <= 0)
            break;
        start_pos = indices[1];
        start_match = @intCast(usize, indices[2]);
        end_match = @intCast(usize, indices[3]);
        match_buffer[current_match] = start_match;
        match_buffer[current_match+1] = end_match;
        current_match += 2;
    }

    var matches: [] usize = Allocator.alloc(usize, current_match)
        catch return MalError.SystemError;
    var i: usize = 0;
    while(i < current_match) {
        matches[i] = match_buffer[i];
        i += 1;
    }

    return matches;
}
