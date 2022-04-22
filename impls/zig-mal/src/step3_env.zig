const std = @import("std");
const Allocator = std.mem.Allocator;

const Env = @import("./env.zig").Env;
const printer = @import("./printer.zig");
const reader = @import("./reader.zig");
const types = @import("./types.zig");
const MalType = types.MalType;
const MalValue = types.MalValue;

const input_buffer_length = 256;
const prompt = "user> ";

// base environment functions

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
    // TODO: use std.math.divFloor/divTrunc for runtime errors instead of
    // undefined behavior when dividing by zero
    return @divFloor(a, b);
}

fn READ(allocator: Allocator, input: []const u8) !MalType {
    const ast = try reader.read_str(allocator, input);
    return ast;
}

const EvalError = error{
    EvalDefInvalidOperands,
    EvalLetInvalidOperands,
    EvalInvalidOperand,
    EvalInvalidOperands,
    EvalInvalidSymbol,
    EnvSymbolNotFound,
} || Allocator.Error;

fn eval_ast(allocator: Allocator, ast: *const MalType, env: *Env) EvalError!MalValue {
    switch (ast.*) {
        .atom => |atom| return switch (atom) {
            .symbol => |symbol| env.get(symbol),
            else => MalValue{ .mal_type = ast.* },
        },
        .list => |list| {
            var results = try MalValue.initListCapacity(allocator, list.items.len);
            for (list.items) |item| {
                const result = try EVAL(allocator, &item, env);
                try results.list.append(result);
            }
            return results;
        },
    }
}

fn EVAL(allocator: Allocator, ast: *const MalType, env: *Env) EvalError!MalValue {
    switch (ast.*) {
        .list => |list| if (list.items.len == 0) return MalValue{ .mal_type = ast.* } else {
            const symbol = list.items[0].asSymbol() catch return error.EvalInvalidSymbol;

            // apply phase

            if (std.mem.eql(u8, symbol, "def!")) {
                const rest = list.items[1..];
                if (rest.len != 2) return error.EvalDefInvalidOperands;
                const key_symbol = rest[0].asSymbol() catch return error.EvalDefInvalidOperands;

                const evaled_value = try EVAL(allocator, &rest[1], env);
                try env.set(key_symbol, evaled_value);
                return evaled_value;
            }

            if (std.mem.eql(u8, symbol, "let*")) {
                const rest = list.items[1..];
                if (rest.len != 2) return error.EvalLetInvalidOperands;
                const bindings = rest[0].asList() catch return error.EvalLetInvalidOperands;
                if (@mod(bindings.items.len, 2) != 0) return error.EvalLetInvalidOperands;

                var let_env = Env.init(allocator, env);
                var i: usize = 0;
                while (i < bindings.items.len) : (i += 2) {
                    const current_bindings = bindings.items[i .. i + 2];
                    const key_symbol = current_bindings[0].asSymbol() catch return error.EvalDefInvalidOperands;
                    const evaled_value = try EVAL(allocator, &current_bindings[1], &let_env);
                    try let_env.set(key_symbol, evaled_value);
                }
                const evaled_value = try EVAL(allocator, &rest[1], &let_env);
                return evaled_value;
            }

            const evaled_ast = try eval_ast(allocator, ast, env);
            const evaled_items = evaled_ast.list.items;

            const fn_symbol = evaled_items[0].asFunction() catch return error.EvalInvalidSymbol;

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
        else => return eval_ast(allocator, ast, env),
    }
}

fn PRINT(allocator: Allocator, ast: *const MalType) ![]const u8 {
    const output = try printer.pr_str(allocator, ast);
    return output;
}

fn rep(allocator: Allocator, input: []const u8, env: *Env) ![]const u8 {
    const ast = try READ(allocator, input);
    const result = try EVAL(allocator, &ast, env);

    // var env_it = env.data.iterator();
    // while (env_it.next()) |entry| {
    //     std.debug.print("env {*} = {}\n", .{ entry.key, entry.value });
    // }

    std.debug.assert(result == .mal_type);
    const output = try PRINT(allocator, &result.mal_type);
    return output;
}

pub fn main() anyerror!void {
    // general purpose allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // REPL environment
    var env = Env.init(gpa.allocator(), null);
    defer env.deinit();

    try env.set("+", MalValue.makeFunction(add));
    try env.set("-", MalValue.makeFunction(subtract));
    try env.set("*", MalValue.makeFunction(multiply));
    try env.set("/", MalValue.makeFunction(divide));

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
        if (rep(arena.allocator(), line, &env)) |result|
            try stdout.print("{s}\n", .{result})
        else |err| {
            const message = switch (err) {
                error.EndOfInput => "unexpected end of input",
                error.ListNoClosingTag => "unbalanced list form, missing closing ')'",
                error.StringLiteralNoClosingTag => "unbalanced string literal, missing closing '\"'",
                error.TokensPastFormEnd => "found additional tokens past end of form",
                error.EvalDefInvalidOperands => "Invalid def! operands",
                error.EvalLetInvalidOperands => "Invalid let* operands",
                error.EvalInvalidOperand => "Invalid operand",
                error.EvalInvalidOperands => "Invalid operands, wrong function argument arity",
                error.EvalInvalidSymbol => "tried to evaluate list where the first item is not a known function symbol or special form",
                error.EnvSymbolNotFound => "symbol not found",
                error.OutOfMemory => "out of memory",
            };
            try stderr.print("Error: {s}\n", .{message});
        }
    }
}
