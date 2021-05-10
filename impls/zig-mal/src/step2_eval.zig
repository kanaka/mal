const std = @import("std");
const Allocator = std.mem.Allocator;

const printer = @import("./printer.zig");
const reader = @import("./reader.zig");
const types = @import("./types.zig");
const MalType = types.MalType;
const MalTypeEval = types.MalTypeEval;

const input_buffer_length = 256;
const prompt = "user> ";

fn READ(allocator: *Allocator, input: []const u8) !MalType {
    const form = try reader.read_str(allocator, input);
    return form;
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

fn eval_ast(allocator: *Allocator, ast: *const MalType, repl_env: ReplEnv) EvalError!MalTypeEval {
    return switch (ast.*) {
        .atom => |atom| switch (atom) {
            .number => |number| MalTypeEval{ .mal_type = ast.* },
            .symbol => |symbol| MalTypeEval{ .function = .{ .op_2_number = (repl_env.get(symbol) orelse return error.UnknownSymbol) } },
        },
        .list => |list| blk: {
            var results = try MalTypeEval.initListCapacity(allocator, list.items.len);
            for (list.items) |item| {
                const result = try EVAL(allocator, &item, repl_env);
                try results.list.append(result);
            }
            break :blk results;
        },
    };
}

fn EVAL(allocator: *Allocator, form: *const MalType, repl_env: ReplEnv) !MalTypeEval {
    var result = try allocator.create(MalTypeEval);
    result.* = switch (form.*) {
        .list => |list| if (list.items.len == 0) MalTypeEval{ .mal_type = form.* } else blk: {
            const evaled_ast = try eval_ast(allocator, form, repl_env);
            const evaled_items = evaled_ast.list.items;

            const fn_symbol = evaled_items[0].asFunction() catch return error.EvalInvalidFunctionSymbol;

            const fn_args = evaled_items[1..];
            // TODO: can probably be compile-time generated from function type info
            break :blk switch (fn_symbol) {
                .op_2_number => |op| fn_blk: {
                    if (fn_args.len != 2) return error.EvalInvalidOperands;
                    const a = fn_args[0].asNumber() catch return error.EvalInvalidOperand;
                    const b = fn_args[1].asNumber() catch return error.EvalInvalidOperand;
                    break :fn_blk MalTypeEval{ .mal_type = MalType.initNumber(op(a, b)) };
                },
            };
        },
        else => try eval_ast(allocator, form, repl_env),
    };
    return result.*;
}

fn PRINT(allocator: *Allocator, form: *const MalType) ![]const u8 {
    const output = try printer.pr_str(allocator, form);
    return output;
}

fn rep(allocator: *Allocator, input: []const u8, repl_env: ReplEnv) ![]const u8 {
    const form = try READ(allocator, input);
    const result = try EVAL(allocator, &form, repl_env);
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
    var repl_env = ReplEnv.init(&gpa.allocator);
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
        var arena = std.heap.ArenaAllocator.init(&gpa.allocator);
        defer arena.deinit();

        // read-eval-print
        if (rep(&arena.allocator, line, repl_env)) |result|
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
