const std = @import("std");
const Allocator = std.mem.Allocator;

const core = @import("./core.zig");
const Env = @import("./env.zig").Env;
const printer = @import("./printer.zig");
const reader = @import("./reader.zig");
const types = @import("./types.zig");
const EvalError = types.EvalError;
const MalType = types.MalType;

const input_buffer_length = 256;
const prompt = "user> ";

fn READ(allocator: Allocator, input: []const u8) !*MalType {
    return reader.read_str(allocator, input);
}

fn EVAL(allocator: Allocator, ast: *MalType, env: *Env) EvalError!*MalType {
    var current_ast = ast;
    var current_env = env;
    while (true) {
        switch (current_ast.*) {
            .list => |list| if (list.items.len == 0) return MalType.makeListEmpty(allocator) else {
                // apply phase
                const first = list.items[0];
                if (first.* == .symbol) {
                    const symbol = first.symbol;

                    if (std.mem.eql(u8, symbol.value, "def!")) {
                        const rest = list.items[1..];
                        if (rest.len != 2) return error.EvalDefInvalidOperands;
                        const key_symbol = rest[0].asSymbol() catch return error.EvalDefInvalidOperands;

                        const evaled_value = try EVAL(allocator, rest[1], current_env);
                        // TODO check this
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
                            const evaled_value = try EVAL(allocator, current_bindings[1], let_env);
                            // TODO: check this
                            try let_env.set(key_symbol.value, evaled_value);
                        }
                        current_ast = rest[1];
                        current_env = let_env;
                        continue;
                    }

                    if (std.mem.eql(u8, symbol.value, "if")) {
                        const rest = list.items[1..];
                        if (rest.len != 2 and rest.len != 3) return error.EvalIfInvalidOperands;
                        const condition = rest[0];
                        const evaled_value = try EVAL(allocator, condition, current_env);
                        if (evaled_value.isTruthy())
                            current_ast = rest[1]
                        else if (rest.len == 3)
                            current_ast = rest[2]
                        else
                            current_ast = try MalType.makeNil(allocator);
                        continue;
                    }

                    if (std.mem.eql(u8, symbol.value, "do")) {
                        const do_len = list.items.len - 1;
                        if (do_len < 1) return error.EvalDoInvalidOperands;
                        const do_ast = try MalType.makeListFromSlice(allocator, list.items[1..do_len]);
                        _ = try eval_ast(allocator, do_ast, current_env);
                        current_ast = list.items[do_len];
                        continue;
                    }

                    if (std.mem.eql(u8, symbol.value, "fn*")) {
                        const parameters = list.items[1].asList() catch return error.EvalInvalidFnParamsList;
                        // convert from a list of MalType to a list of valid symbol keys to use in environment init
                        var binds = try std.ArrayList(MalType.Symbol).initCapacity(allocator, parameters.items.len);
                        for (parameters.items) |parameter| {
                            const parameter_symbol = parameter.asSymbol() catch return error.EvalInvalidFnParamsList;
                            binds.appendAssumeCapacity(parameter_symbol);
                        }
                        return MalType.makeClosure(allocator, .{
                            .parameters = binds,
                            .body = list.items[2],
                            .env = current_env,
                            .eval = EVAL,
                        });
                    }
                }
                const evaled_ast = try eval_ast(allocator, current_ast, current_env);
                const evaled_items = evaled_ast.list.items;

                const function = evaled_items[0];
                const args = evaled_items[1..];

                switch (function.*) {
                    .primitive => |primitive| return primitive.apply(allocator, args),
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
                        current_ast = closure.body;
                        current_env = fn_env_ptr;
                        continue;
                    },
                    else => return error.EvalNotSymbolOrFn,
                }
            },
            else => return eval_ast(allocator, current_ast, current_env),
        }
    }
}

fn eval_ast(allocator: Allocator, ast: *MalType, env: *Env) EvalError!*MalType {
    return switch (ast.*) {
        // TODO: check this
        .symbol => |symbol| try env.get(symbol.value),
        .list => |list| blk: {
            var results = try MalType.makeListCapacity(allocator, list.items.len);
            for (list.items) |item| {
                const result = try EVAL(allocator, item, env);
                results.list.appendAssumeCapacity(result);
            }
            break :blk results;
        },
        else => ast,
    };
}

fn PRINT(allocator: Allocator, ast: *const MalType) ![]const u8 {
    const output = try printer.pr_str(allocator, ast, true);
    return output;
}

fn rep(global_allocator: Allocator, allocator: Allocator, input: []const u8, env: *Env) ![]const u8 {
    const ast = try READ(global_allocator, input);
    const result = try EVAL(global_allocator, ast, env);
    const output = try PRINT(allocator, result);
    return output;
}

var repl_env: Env = undefined;

fn eval(allocator: Allocator, ast: *MalType) EvalError!*MalType {
    return EVAL(allocator, ast, &repl_env);
}

pub fn main() anyerror!void {
    // general purpose allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // global arena allocator
    var global_arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer global_arena.deinit();
    const gaa = global_arena.allocator();

    // REPL environment
    repl_env = Env.init(gpa.allocator(), null);
    defer repl_env.deinit();

    inline for (@typeInfo(@TypeOf(core.ns)).Struct.fields) |field| {
        try repl_env.set(field.name, try MalType.makePrimitive(gaa, @field(core.ns, field.name)));
    }

    try repl_env.set("eval", try MalType.makePrimitive(gaa, eval));

    var input_buffer: [input_buffer_length]u8 = undefined;
    // initialize std io reader and writer
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // evaluate global prelude/preamble-type expressions
    _ = try rep(gaa, gaa, "(def! not (fn* (a) (if a false true)))", &repl_env);
    _ = try rep(gaa, gaa, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", &repl_env);

    // read command line arguments
    var iter = std.process.args();
    // ignore first argument (executable file)
    _ = iter.next();
    const opt_filename = iter.next();
    // call (load-file filename) if given a filename argument
    if (opt_filename) |filename| {
        const string = try std.mem.join(gaa, "\"", &.{ "(load-file ", filename, ")" });
        _ = try rep(gaa, gaa, string, &repl_env);
        return;
    }

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
        // local arena allocator, memory is freed at end of loop iteration
        var arena = std.heap.ArenaAllocator.init(gpa.allocator());
        defer arena.deinit();

        // read-eval-print
        if (rep(gaa, arena.allocator(), line, &repl_env)) |result|
            try stdout.print("{s}\n", .{result})
        else |err| {
            const message = switch (err) {
                error.EmptyInput => continue,
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
