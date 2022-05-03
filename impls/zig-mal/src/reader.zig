const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("./types.zig");
const MalType = types.MalType;
const Atom = MalType.Atom;
const replaceMultipleOwned = @import("./utils.zig").replaceMultipleOwned;

const TokenList = std.ArrayList([]const u8);

const Reader = struct {
    const Self = @This();

    tokens: TokenList,
    position: u16,

    pub fn init(tokens: TokenList) Self {
        return Self{
            .tokens = tokens,
            .position = 0,
        };
    }

    pub fn next(self: *Self) ?[]const u8 {
        if (self.position >= self.tokens.items.len) {
            return null;
        }
        const token = self.tokens.items[self.position];
        self.position += 1;
        return token;
    }

    pub fn peek(self: *Self) ?[]const u8 {
        if (self.position >= self.tokens.items.len) {
            return null;
        }
        return self.tokens.items[self.position];
    }
};

pub const ReadError = error{
    EndOfInput,
    ListNoClosingTag,
    StringLiteralNoClosingTag,
    TokensPastFormEnd,
} || Allocator.Error;

pub fn read_str(allocator: Allocator, input: []const u8) !MalType {
    // tokenize input string into token list
    const tokens = try tokenize(allocator, input);

    // create a Reader instance with the tokens list
    var reader = Reader.init(tokens);
    // read a mal form
    const form = (try read_form(allocator, &reader)) orelse error.EndOfInput;
    // check if there are still remaining tokens after the form is read
    if (reader.peek()) |token| blk: {
        // check if the token is a comment, in which case there is no error
        if (token[0] == ';') {
            break :blk;
        }
        // there should only be a single top-level form, so any remaining token
        // (non-comment) indicates an error
        return error.TokensPastFormEnd;
    }
    return form;
}

fn read_form(allocator: Allocator, reader: *Reader) ReadError!?MalType {
    return if (reader.peek()) |first_token|
        switch (first_token[0]) {
            '(' => try read_list(allocator, reader),
            ')' => null,
            else => try read_atom(allocator, reader),
        }
    else
        error.EndOfInput;
}

fn read_list(allocator: Allocator, reader: *Reader) !MalType {
    // skip over the first '(' token in the list
    _ = reader.next();
    var list = std.ArrayList(MalType).init(allocator);
    // read the next forms until a matching ')' is found, or error otherwise
    var err_form = read_form(allocator, reader);
    while (err_form) |opt_form| : (err_form = read_form(allocator, reader)) {
        if (opt_form) |form| {
            // push valid forms into array list
            try list.append(form);
        } else {
            // found matching ')', break loop
            break;
        }
        // no matching closing ')' parenthes, return error
    } else |_| return error.ListNoClosingTag;
    // skip over the last ')' token in the list
    _ = reader.next();
    return MalType{ .list = list };
}

fn read_atom(allocator: Allocator, reader: *Reader) !MalType {
    return MalType{
        .atom = if (reader.next()) |token|
            // TODO: support keyword
            if (std.mem.eql(u8, token, "nil"))
                .nil
            else if (std.mem.eql(u8, token, "true"))
                .t
            else if (std.mem.eql(u8, token, "false"))
                .f
            else if (token[0] == '"')
                Atom{ .string = .{ .value = try replaceEscapeSequences(allocator, token[1 .. token.len - 1]), .allocator = allocator } }
            else if (std.fmt.parseInt(i32, token, 10)) |int|
                Atom{ .number = int }
            else |_|
                Atom{ .symbol = .{ .value = token, .allocator = allocator } }
        else
            return error.EndOfInput,
    };
}

fn replaceEscapeSequences(allocator: Allocator, str: []const u8) ![]const u8 {
    // replace \" with "
    // replace \\ with \
    // replace \n with newline character
    const needles = .{ "\\\"", "\\\\", "\\n" };
    const replacements = .{ "\"", "\\", "\n" };
    return replaceMultipleOwned(u8, 3, allocator, str, needles, replacements);
}

// [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)

// For each match captured within the parenthesis starting at char 6 of the regular expression a
// new token will be created.

// [\s,]*: Matches any number of whitespaces or commas. This is not captured so it will be
// ignored and not tokenized.

// ~@: Captures the special two-characters ~@ (tokenized).

// [\[\]{}()'`~^@]: Captures any special single character, one of []{}()'`~^@ (tokenized).

// "(?:\\.|[^\\"])*"?: Starts capturing at a double-quote and stops at the next double-quote
// unless it was preceded by a backslash in which case it includes it until the next
// double-quote (tokenized). It will also match unbalanced strings (no ending double-quote)
// which should be reported as an error.

// ;.*: Captures any sequence of characters starting with ; (tokenized).

// [^\s\[\]{}('"`,;)]*: Captures a sequence of zero or more non special characters (e.g.
// symbols, numbers, "true", "false", and "nil") and is sort of the inverse of the one above
// that captures special characters (tokenized).
fn tokenize(allocator: Allocator, input: []const u8) !TokenList {
    var tokens = TokenList.init(allocator);

    const State = enum {
        start,
        comment,
        other,
        string_literal,
        string_literal_backslash,
        tilde,
    };
    var state: State = .start;
    var start_index: usize = undefined;

    var index: usize = 0;
    while (index < input.len) : (index += 1) {
        const c = input[index];
        switch (state) {
            .start => switch (c) {
                ' ', '\n', '\t', '\r', ',' => {},
                '"' => {
                    state = .string_literal;
                    start_index = index;
                },
                '~' => {
                    state = .tilde;
                    start_index = index;
                },
                '[', ']', '{', '}', '(', ')', '\'', '`', '^', '@' => {
                    try tokens.append(input[index .. index + 1]);
                },
                ';' => {
                    state = .comment;
                    start_index = index;
                },
                else => {
                    state = .other;
                    start_index = index;
                },
            },
            .tilde => switch (c) {
                '@' => {
                    try tokens.append("~@");
                    state = .start;
                    start_index = undefined;
                },
                else => {
                    try tokens.append("~");
                    // backtrack with .start state
                    index -= 1;
                    state = .start;
                    start_index = undefined;
                },
            },
            .string_literal => switch (c) {
                '"' => {
                    try tokens.append(input[start_index .. index + 1]);
                    state = .start;
                    start_index = undefined;
                },
                '\\' => {
                    state = .string_literal_backslash;
                },
                else => {},
            },
            .string_literal_backslash => switch (c) {
                '"' => {
                    state = .string_literal;
                },
                else => {
                    state = .string_literal;
                },
            },
            .comment => switch (c) {
                '\n' => {
                    try tokens.append(input[start_index..index]);
                    state = .start;
                    start_index = undefined;
                },
                else => {},
            },
            .other => switch (c) {
                ' ', '\n', '\t', '\r', ',', '"', '~', '[', ']', '{', '}', '(', ')', '\'', '`', '^', ';' => {
                    try tokens.append(input[start_index..index]);
                    // backtrack with .start state
                    index -= 1;
                    state = .start;
                    start_index = undefined;
                },
                else => {},
            },
        }
    }
    switch (state) {
        .start => {},
        .comment, .other, .tilde => {
            try tokens.append(input[start_index..index]);
        },
        .string_literal, .string_literal_backslash => {
            return error.StringLiteralNoClosingTag;
        },
    }
    return tokens;
}
