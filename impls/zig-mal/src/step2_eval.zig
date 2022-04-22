const std = @import("std");
const Allocator = std.mem.Allocator;

const printer = @import("./printer.zig");
const reader = @import("./reader.zig");
const types = @import("./types.zig");
const MalType = types.MalType;
const MalValue = types.MalValue;

const input_buffer_length = 256;
const prompt = "user> ";

fn READ(allocator: Allocator, input: []const u8) !MalType {
    const ast = try reader.read_str(allocator, input);
    return ast;
}

fn add(a: i32, b: i32) i32 {
    return a + b;
}

fn subtract(a: i32, b: i32) i32 {
    return a - b;
}

fn multiply(a: i32, b: i32) i32 {
    return a * b;
}

fn divide(a: i32, b: i32) i32 {
    // TODO: use std.math.divFloor/divTrunc?
    return @divFloor(a, b);
}

const fn_type = fn (a: i32, b: i32) i32;
const ReplEnv = std.StringHashMap(fn_type);

const EvalError = error{
    EvalInvalidOperand,
    EvalInvalidOperands,
    EvalInvalidFunctionSymbol,
    UnknownSymbol,
} || Allocator.Error;

fn eval_ast(allocator: Allocator, ast: *const MalType, repl_env: ReplEnv) EvalError!MalValue {
    switch (ast.*) {
        .atom => |atom| return switch (atom) {
            .symbol => |symbol| MalValue{ .function = .{ .op_2_number = (repl_env.get(symbol) orelse return error.UnknownSymbol) } },
            else => MalValue{ .mal_type = ast.* },
        },
        .list => |list| {
            var results = try MalValue.initListCapacity(allocator, list.items.len);
            for (list.items) |item| {
                const result = try EVAL(allocator, &item, repl_env);
                try results.list.append(result);
            }
            return results;
        },
    }
}

fn EVAL(allocator: Allocator, ast: *const MalType, repl_env: ReplEnv) !MalValue {
    switch (ast.*) {
        .list => |list| if (list.items.len == 0) return MalValue{ .mal_type = ast.* } else {
            const evaled_ast = try eval_ast(allocator, ast, repl_env);
            const evaled_items = evaled_ast.list.items;

            const fn_symbol = evaled_items[0].asFunction() catch return error.EvalInvalidFunctionSymbol;

            const fn_args = evaled_items[1..];
            // TODO: can probably be compile-time generated from function type info
            switch (fn_symbol) {
                .op_2_number => |op| {
                    if (fn_args.len != 2) return error.EvalInvalidOperands;
                    const a = fn_args[0].asNumber() catch return error.EvalInvalidOperand;
                    const b = fn_args[1].asNumber() catch return error.EvalInvalidOperand;
                    return MalValue{ .mal_type = MalType.makeNumber(op(a, b)) };
                },
            }
        },
        else => return eval_ast(allocator, ast, repl_env),
    }
}

fn PRINT(allocator: Allocator, ast: *const MalType) ![]const u8 {
    const output = try printer.pr_str(allocator, ast);
    return output;
}

fn rep(allocator: Allocator, input: []const u8, repl_env: ReplEnv) ![]const u8 {
    const ast = try READ(allocator, input);
    const result = try EVAL(allocator, &ast, repl_env);
    std.debug.assert(result == .mal_type);
    const output = try PRINT(allocator, &result.mal_type);
    return output;
}

pub fn main() anyerror!void {
    // general purpose allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // REPL environment
    // TODO: use AutoHashMap or other HashMap variant?
    var repl_env = ReplEnv.init(gpa.allocator());
    defer repl_env.deinit();
    try repl_env.put("+", add);
    try repl_env.put("-", subtract);
    try repl_env.put("*", multiply);
    try repl_env.put("/", divide);

    var input_buffer: [input_buffer_length]u8 = undefined;
    // initialize std io reader and writer
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    // main repl loop
    while (true) {
        // print prompt
        // TODO: line editing and history, (readline, editline, linenoise)
        try stdout.print(prompt, .{});
        // read line of input
        const line = (try stdin.readUntilDelimiterOrEof(&input_buffer, '\n')) orelse {
            // reached input end-of-file
            break;
        };
        // arena allocator, memory is freed at end of loop iteration
        var arena = std.heap.ArenaAllocator.init(gpa.allocator());
        defer arena.deinit();

        // read-eval-print
        if (rep(arena.allocator(), line, repl_env)) |result|
            try stdout.print("{s}\n", .{result})
        else |err| {
            const message = switch (err) {
                error.EndOfInput => "unexpected end of input",
                error.ListNoClosingTag => "unbalanced list form, missing closing ')'",
                error.StringLiteralNoClosingTag => "unbalanced string literal, missing closing '\"'",
                error.TokensPastFormEnd => "found additional tokens past end of form",
                error.EvalInvalidOperand => "Invalid operand",
                error.EvalInvalidOperands => "Invalid operands, wrong function argument arity",
                error.EvalInvalidFunctionSymbol => "tried to evaluate list where the first item is not a known function symbol",
                error.UnknownSymbol => "tried to evaluate unknown symbol",
                error.OutOfMemory => "out of memory",
            };
            try stderr.print("Error: {s}\n", .{message});
        }
    }
}
