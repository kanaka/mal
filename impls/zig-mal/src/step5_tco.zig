const std = @import("std");
const Allocator = std.mem.Allocator;

const core = @import("./core.zig");
const Env = @import("./env.zig").Env;
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

const EvalError = error{
    EvalDefInvalidOperands,
    EvalDoInvalidOperands,
    EvalIfInvalidOperands,
    EvalLetInvalidOperands,
    EvalInvalidOperand,
    EvalInvalidOperands,
    EvalNotSymbolOrFn,
    EnvSymbolNotFound,
    EvalInvalidFnParamsList,
} || Allocator.Error || MalValue.Function.Primitive.Error;

fn EVAL(allocator: Allocator, ast: *const MalType, env: *Env) EvalError!MalValue {
    var current_ast = ast;
    var current_env = env;
    while (true) {
        switch (current_ast.*) {
            .list => |list| if (list.items.len == 0) return MalValue.initListAlloc(allocator) else {
                // apply phase
                const first = list.items[0];
                if (first == .atom and first.atom == .symbol) {
                    const symbol = first.atom.symbol;

                    if (std.mem.eql(u8, symbol.value, "def!")) {
                        const rest = list.items[1..];
                        if (rest.len != 2) return error.EvalDefInvalidOperands;
                        const key_symbol = rest[0].asSymbol() catch return error.EvalDefInvalidOperands;

                        const evaled_value = try EVAL(allocator, &rest[1], current_env);
                        try current_env.set(key_symbol.value, evaled_value);
                        return evaled_value;
                    }

                    if (std.mem.eql(u8, symbol.value, "let*")) {
                        const rest = list.items[1..];
                        if (rest.len != 2) return error.EvalLetInvalidOperands;
                        const bindings = rest[0].asList() catch return error.EvalLetInvalidOperands;
                        if (@mod(bindings.items.len, 2) != 0) return error.EvalLetInvalidOperands;

                        var let_env = try current_env.initChild();
                        var i: usize = 0;
                        while (i < bindings.items.len) : (i += 2) {
                            const current_bindings = bindings.items[i .. i + 2];
                            const key_symbol = current_bindings[0].asSymbol() catch return error.EvalDefInvalidOperands;
                            const evaled_value = try EVAL(allocator, &current_bindings[1], let_env);
                            try let_env.set(key_symbol.value, evaled_value);
                        }
                        current_ast = &rest[1];
                        current_env = let_env;
                        continue;
                    }

                    if (std.mem.eql(u8, symbol.value, "if")) {
                        const rest = list.items[1..];
                        if (rest.len != 2 and rest.len != 3) return error.EvalIfInvalidOperands;
                        const condition = rest[0];
                        const evaled_value = try EVAL(allocator, &condition, current_env);
                        if (evaled_value.isTruthy())
                            current_ast = &rest[1]
                        else if (rest.len == 3)
                            current_ast = &rest[2]
                        else
                            current_ast = &MalType{ .atom = .nil };
                        continue;
                    }

                    if (std.mem.eql(u8, symbol.value, "do")) {
                        const do_len = list.items.len - 1;
                        if (do_len < 1) return error.EvalDoInvalidOperands;
                        const do_items = list.items[1..];
                        for (do_items[0 .. do_len - 1]) |item| {
                            // _ = try EVAL(allocator, &item, current_env);
                            _ = try eval_ast(allocator, &item, current_env);
                        }
                        current_ast = &do_items[do_len - 1];
                        continue;
                    }

                    if (std.mem.eql(u8, symbol.value, "fn*")) {
                        const parameters = list.items[1].asList() catch return error.EvalInvalidFnParamsList;
                        const body = list.items[2];
                        // convert from a list of MalType to a list of valid symbol keys to use in environment init
                        var binds = try std.ArrayList(MalType.Symbol).initCapacity(allocator, parameters.items.len);
                        for (parameters.items) |parameter| {
                            const parameter_symbol = parameter.asSymbol() catch return error.EvalInvalidFnParamsList;
                            binds.appendAssumeCapacity(parameter_symbol);
                        }
                        // TODO: do we need to copy at this point?
                        return MalValue{
                            .function = .{
                                .closure = .{
                                    .parameters = binds,
                                    .body = body,
                                    .env = current_env,
                                },
                            },
                        };
                    }
                }
                const evaled_ast = try eval_ast(allocator, current_ast, current_env);
                const evaled_items = evaled_ast.list.items;

                const function = evaled_items[0].asFunction() catch return error.EvalNotSymbolOrFn;
                const args = evaled_items[1..];
                switch (function) {
                    .primitive => |primitive| return primitive.eval(allocator, args),
                    .closure => |closure| {
                        const parameters = closure.parameters.items;
                        if (parameters.len != args.len) {
                            return error.EvalInvalidOperands;
                        }
                        // convert from a list of MalType.Symbol to a list of valid symbol keys to use in environment init
                        var binds = try std.ArrayList([]const u8).initCapacity(allocator, parameters.len);
                        for (parameters) |parameter| {
                            binds.appendAssumeCapacity(parameter.value);
                        }
                        var fn_env_ptr = try closure.env.initChildBindExprs(binds.items, args);
                        current_ast = &closure.body;
                        current_env = fn_env_ptr;
                        continue;
                    },
                }
            },
            else => return eval_ast(allocator, current_ast, current_env),
        }
    }
}

fn eval_ast(allocator: Allocator, ast: *const MalType, env: *Env) EvalError!MalValue {
    switch (ast.*) {
        .atom => |atom| return switch (atom) {
            .symbol => |symbol| env.get(symbol.value),
            else => MalValue{ .mal_type = ast.* },
        },
        .list => |list| {
            var results = try MalValue.initListCapacity(allocator, list.items.len);
            for (list.items) |item| {
                const result = try EVAL(allocator, &item, env);
                results.list.appendAssumeCapacity(result);
            }
            return results;
        },
    }
}

fn PRINT(allocator: Allocator, ast: *const MalValue) ![]const u8 {
    const output = try printer.pr_str(allocator, ast, true);
    return output;
}

fn rep(allocator: Allocator, input: []const u8, env: *Env) ![]const u8 {
    const ast = try READ(allocator, input);
    const result = try EVAL(allocator, &ast, env);
    const output = try PRINT(allocator, &result);
    return output;
}

pub fn main() anyerror!void {
    // general purpose allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // REPL environment
    var env = Env.init(gpa.allocator(), null);
    defer env.deinit();

    inline for (@typeInfo(@TypeOf(core.ns)).Struct.fields) |field| {
        try env.set(field.name, @field(core.ns, field.name));
    }

    var input_buffer: [input_buffer_length]u8 = undefined;
    // initialize std io reader and writer
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // temporary allocator to evaluate global prelude/preamble-type expressions
    var ar = std.heap.ArenaAllocator.init(gpa.allocator());
    _ = try rep(ar.allocator(), "(def! not (fn* (a) (if a false true)))", &env);
    ar.deinit();

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
                error.EvalDoInvalidOperands => "Invalid do operands",
                error.EvalIfInvalidOperands => "Invalid if operands",
                error.EvalLetInvalidOperands => "Invalid let* operands",
                error.EvalInvalidFnParamsList => "Invalid parameter list to fn* expression",
                error.EvalInvalidOperand => "Invalid operand",
                error.EvalInvalidOperands => "Invalid operands, wrong function argument arity",
                error.EvalNotSymbolOrFn => "tried to evaluate list where the first item is not a function or special form",
                error.EnvSymbolNotFound => "symbol not found",
                error.OutOfMemory => "out of memory",
                else => @errorName(err),
            };
            try stderr.print("Error: {s}\n", .{message});
            // print error return stack trace in debug build
            // if (@errorReturnTrace()) |trace| {
            //     std.debug.dumpStackTrace(trace.*);
            // }
        }
    }
}
