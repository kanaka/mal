const std = @import("std");

const reader = @import("reader.zig");
const printer = @import("printer.zig");
const getline = @import("readline.zig").getline;
const string_eql = std.hash_map.eqlString;
const hash_map = @import("hmap.zig");
const core = @import("core.zig");

const Allocator = @import("std").heap.c_allocator;

const MalType = @import("types.zig").MalType;
const MalError = @import("error.zig").MalError;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const Env = @import("env.zig").Env;
const get_error_data = @import("error.zig").get_error_data;
const throw = @import("error.zig").throw;
const stdout_file = std.io.getStdOut();

var repl_environment = Env.new_root();

fn READ(a: []const u8) !*MalType {
    var read = try reader.read_str(a);
    return reader.read_form(&read);
}

// Do not allocate this one on each EVAL run.
// The string is static, but will never be deallocated.
var DEBUG_EVAL = MalType { .Symbol = .{ .data = "DEBUG-EVAL" } };

fn EVAL(mal_arg: *MalType, env_arg: *Env, finally_destroy_env: bool) MalError!*MalType {
    var mal = mal_arg;
    var env = env_arg;
    var fde = finally_destroy_env;
    defer if(fde) env.decref();
    while(true) {

        if(try env.get(&DEBUG_EVAL)) |dbgeval| {
            switch (dbgeval.*) {
                .Nil, .False => {},
                else => {
                    try stdout_file.writeAll("EVAL: ");
                    try PRINT(mal.*);
                }
            }
        }

        switch(mal.*) {
            .List => |ll| {
                const items = ll.data.items;
                if(items.len == 0) {
                    mal.incref();
                    return mal;
                }
                const first_mal = items[0];
                const symbol = switch(first_mal.*) {
                    .Symbol => |symbol| symbol.data,
                    else => "",
                };
                if(string_eql(symbol, "def!")) {
                    return EVAL_def(items[1..], env);
                }
                else if(string_eql(symbol, "defmacro!")) {
                    return EVAL_defmacro(items[1..], env);
                }
                else if(string_eql(symbol, "let*")) {
                    try EVAL_let(items[1..], &mal, &env, &fde);
                    continue;
                }
                else if(string_eql(symbol, "do")) {
                    try EVAL_do(items[1..], &mal, env);
                    continue;
                }
                else if(string_eql(symbol, "if")) {
                    try EVAL_if(items[1..], &mal, env);
                    continue;
                }
                else if(string_eql(symbol, "fn*")) {
                    return EVAL_fn(items[1..], env);
                }
                else if(string_eql(symbol, "quote")) {
                    return EVAL_quote(items[1..]);
                }
                else if(string_eql(symbol, "quasiquote")) {
                    if(items.len != 2) return MalError.ArgError;
                    const second = items[1];
                    mal = try quasiquote(second);
                    continue;
                }
                else if(string_eql(symbol, "try*")) {
                    return EVAL_try(items[1..], env);
                }
                else {
                    const evaluated_first = try EVAL(first_mal, env, false);
                    defer evaluated_first.decref();
                    switch (evaluated_first.*) {
                        .Func => |func_data| {
                            if(func_data.is_macro) {
                                mal = try apply_function(evaluated_first.*, items[1..]);
                                continue;
                            }
                        },
                        else => {}
                    }
                    // A slice would be sufficient, but a List is convenient
                    // for partial deallocation in case of error.
                    const args = try MalType.new_list();
                    defer args.decref();
                    for(items[1..]) |x| {
                        const new_item = try EVAL(x, env, false);
                        try args.List.data.append(Allocator, new_item);
                    }
                   switch(evaluated_first.*) {
                        .Func => |func_data| {
                            if(fde) {
                                env.decref();
                            }
                            else {
                                fde = true;
                            }
                            env = try func_data.gen_env(args.List.data.items);
                            mal = func_data.body;
                            continue;
                        },
                        else => {},
                    }
                    return apply_function(evaluated_first.*, args.List.data.items);
                }
            },
            .Symbol => {
                return EVAL_symbol(mal, env);
            },
            .Vector => |ll| {
                return EVAL_vector(ll.data.items, env);
            },
            .HashMap => |hmap| {
                return EVAL_map(hmap.data, env);
            },
            else => {
                mal.incref();
                return mal;
            },
        }
    }
}

fn eval(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const a1 = args[0];
    return EVAL(a1, &repl_environment, false);
}

fn starts_with(mal: MalType, sym: []const u8) ?*MalType {
    const ll = switch(mal) {
        .List => |l| l,
        else => return null,
    };
    const items = ll.data.items;
    if(items.len != 2) {
        return null;
    }
    const ss = switch(items[0].*) {
        .Symbol => |s| s,
        else => return null,
    };
    if(string_eql(ss.data, sym)) {
        return items[1];
    }
    return null;
}

fn EVAL_def(args: []*MalType, env: *Env) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const symbol_name = args[0];
    const second_arg = args[1];
    const new_value = try EVAL(second_arg, env, false);
    try env.set(symbol_name, new_value);
    new_value.incref();
    return new_value;
}

fn EVAL_defmacro(args: []*MalType, env: *Env) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const symbol_name = args[0];
    const second_arg = args[1];
    const new_value = try EVAL(second_arg, env, false);
    errdefer new_value.decref();
    const f = switch (new_value.*) {
        .Func => |func_data| func_data,
        else => return MalError.TypeError,
    };
    const macro = try MalType.newFunc(f.arg_list, f.body, f.environment);
    f.arg_list.incref();
    f.body.incref();
    f.environment.incref();
    macro.Func.is_macro = true;
    try env.set(symbol_name, macro);
    macro.incref();
    return macro;
}

fn EVAL_let(args: []*MalType, mal_ptr: **MalType, env_ptr: **Env, fde: *bool) !void {
    if(args.len != 2) return MalError.ArgError;
    const env = env_ptr.*;
    const binding_arg = args[0];
    const eval_arg = args[1];
    const binds = try binding_arg.as_slice();
    if(binds.len % 2 != 0) return MalError.ArgError;
    const new_env = try Env.new(env);
    //  Change env and fde in case an error occurs later in this procedure
    //  and fde triggers an env.decref() at the exit of EVAL.
    if(!fde.*) {
        env.incref();
        fde.* = true;
    }
    env_ptr.* = new_env;
    for(0..binds.len / 2) |i| {
        const key = binds[2*i];
        const val_mal = binds[2*i + 1];
        const evaled_mal = try EVAL(val_mal, new_env, false);
        errdefer evaled_mal.decref();
        try new_env.set(key, evaled_mal);
        //  Do not increment the refcount for the value.
    }
    mal_ptr.* = eval_arg;
}

fn EVAL_do(args: []*MalType, mal_ptr: **MalType, env: *Env) !void {
    if(args.len == 0) return MalError.ArgError;
    const last_mal = args[args.len - 1];
    for (args[0..args.len - 1]) |form| {
        const item = try EVAL(form, env, false);
        item.decref();
    }
    mal_ptr.* = last_mal;
}

fn EVAL_if(args: []*MalType, mal_ptr: **MalType, env: *Env) !void {
    if(args.len != 2 and args.len != 3) return MalError.ArgError;
    const first_arg = args[0];
    const evaled = try EVAL(first_arg, env, false);
    const is_true = switch(evaled.*) {
        .False => false,
        .Nil => false,
        else => true,
    };
    evaled.decref();
    if(is_true) {
        const second_arg = args[1];
        mal_ptr.* = second_arg;
        return;
    }
    if(args.len == 2) {
        mal_ptr.* = &MalType.NIL;
        return;
    }
    const third_arg = args[2];
    mal_ptr.* = third_arg;
}

fn EVAL_fn(args: []*MalType, env: *Env) !*MalType {
    if(args.len != 2) return MalError.ArgError;
    const arg_mal = args[0];
    const body_mal = args[1];
    for (try arg_mal.as_slice()) |x| {
        switch (x.*) {
            .Symbol => {},
            else => return MalError.TypeError,
        }
    }
    const new_func = try MalType.newFunc(arg_mal, body_mal, env);
    arg_mal.incref();
    body_mal.incref();
    env.incref();
    return new_func;
}

fn EVAL_quote(args: []*MalType) !*MalType {
    if(args.len != 1) return MalError.ArgError;
    const quoted = args[0];
    quoted.incref();
    return quoted;
}

fn EVAL_try(args: []*MalType, env: *Env) !*MalType {
    if(args.len != 1 and args.len != 2) return MalError.ArgError;
    const mal_to_try = args[0];
    if(args.len == 1) {
        return EVAL(mal_to_try, env, false);
    }
    const catch_mal = args[1];
    const catch_list = switch (catch_mal.*) {
        .List => |l| l.data.items,
        else => return MalError.TypeError,
    };
    if(catch_list.len != 3) return MalError.ArgError;
    switch (catch_list[0].*) {
        .Symbol => |s| {
            if(!string_eql(s.data, "catch*")) return MalError.ArgError;
        },
        else => return MalError.ArgError,
    }

    const evaled_mal = EVAL(mal_to_try, env, false) catch |err| {
        const err_symbol = catch_list[1];
        const err_body = catch_list[2];
        const err_val = get_error_data()
            orelse try MalType.new_string(@errorName(err), true);
        const new_env = try Env.new(env);
        env.incref();
        defer new_env.decref();
        try new_env.set(err_symbol, err_val); // no incref for err_val.
        const result = EVAL(err_body, new_env, false);
        return result;
    };
    return evaled_mal;
}

fn quasiquote(ast: *MalType) MalError!*MalType {
  switch (ast.*) {
    .Symbol, .HashMap => {
        const new_list = try MalType.new_list();
        errdefer new_list.decref();
        try new_list.List.data.append(Allocator, try MalType.new_symbol("quote", true));
        try new_list.List.data.append(Allocator, ast);
        ast.incref();
        return new_list;
    },
    .List => |l| {
        if(starts_with(ast.*, "unquote")) |unquoted| {
            unquoted.incref();
            return unquoted;
        }
        return try qq_loop(l.data.items);
    },
    .Vector => |l| {
        const new_list = try MalType.new_list();
        errdefer new_list.decref();
        try new_list.List.data.append(Allocator, try MalType.new_symbol("vec", true));
        try new_list.List.data.append(Allocator, try qq_loop(l.data.items));
        return new_list;
    },
    else => {
        ast.incref();
        return ast;
    },
  }
}

fn qq_loop(items: []*MalType) !*MalType {
    var result = try MalType.new_list();
    errdefer result.decref();
    for (0..items.len) |i| {
        const elt = items[items.len - 1 - i];
        const new_list = try MalType.new_list();
        errdefer new_list.decref();
        if(starts_with(elt.*, "splice-unquote")) |unquoted| {
            try new_list.List.data.append(Allocator, try MalType.new_symbol("concat", true));
            try new_list.List.data.append(Allocator, unquoted);
            unquoted.incref();
        }
        else {
            try new_list.List.data.append(Allocator, try MalType.new_symbol("cons", true));
            try new_list.List.data.append(Allocator, try quasiquote(elt));
        }
        try new_list.List.data.append(Allocator, result);
        result = new_list;
    }
    return result;
}

fn PRINT(mal: MalType) !void {
    try printer.one_stdout(mal);
    try stdout_file.writeAll("\n");
}

fn rep(print: bool, input: []const u8) !void {
    const read_input = try READ(input);
    defer read_input.decref();
    const eval_input = try EVAL(read_input, &repl_environment, false);
    defer eval_input.decref();
    if(print) {
        try PRINT(eval_input.*);
    }
}

fn EVAL_symbol(mal: *MalType, env: *Env) !*MalType {
    if(try env.get(mal)) |value| {
        value.incref();
        return value;
    }
    const err = try std.fmt.allocPrint(Allocator, "'{s}' not found",
        .{mal.Symbol.data});
    return throw(try MalType.new_string(err, false));
}

fn EVAL_vector(ll: []*MalType, env: *Env) !*MalType {
            const ret_mal = try MalType.new_vector();
            errdefer ret_mal.decref();
            for(ll) |x| {
                const new_mal = try EVAL(x, env, false);
                try ret_mal.Vector.data.append(Allocator, new_mal);
            }
            return ret_mal;
}

fn EVAL_map(hmap: hash_map.MalHashMap, env: *Env) !*MalType {
            const new_hashmap = try MalType.new_hashmap();
            errdefer new_hashmap.decref();
            var iterator = hmap.iterator();
            while(iterator.next()) |pair| {
                const key = pair.key_ptr.*;
                const value = pair.value_ptr.*;
                const evaled_value = try EVAL(value, env, false);
                try hash_map.map_insert_incref_key(&new_hashmap.HashMap.data, key, evaled_value);
            }
            return new_hashmap;
}

fn make_environment() !void {

   for(core.core_namespace) |pair| {
        const name = try MalType.new_symbol(pair.name, true);
        const func_mal = try MalType.newFnCore(pair.func);
        try repl_environment.set(name, func_mal);
        name.decref();
    }

    const eval_sym = try MalType.new_symbol("eval", true);
    const eval_mal = try MalType.newFnCore(eval);
    try repl_environment.set(eval_sym, eval_mal);
    eval_sym.decref();

    const def_not_string: [] const u8 =
        \\(def! not (fn* (a) (if a false true)))
    ;
    try rep(false, def_not_string);

    const load_file_string: [] const u8 =
        \\(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
    ;
    try rep(false, load_file_string);

    const def_cond_macro_string: [] const u8 =
        \\(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))
    ;
    try rep(false, def_cond_macro_string);

    const host_language_sym = try MalType.new_symbol("*host-language*", true);
    const host_language_mal = try MalType.new_string("Zig", true);
    try repl_environment.set(host_language_sym, host_language_mal);
}

fn do_print_header() !void {
    const welcome_msg_cmd: [] const u8 =
        \\(println (str "Mal [" *host-language* "]"))
    ;
    try rep(false, welcome_msg_cmd);
}

pub fn apply_function(f: MalType, args: []*MalType) MalError!*MalType {

    switch(f) {
        .FnCore => |fncoredata| {
            return fncoredata.data(args);
        },
        .Func => |funcdata| {
            const apply_env = try funcdata.gen_env(args);
            defer apply_env.decref();
            return EVAL(funcdata.body, apply_env, false);
        },
        else => {
            return MalError.ApplyError;
        },
    }
}

pub fn main() !void {

    //  Break a circular dependency between modules.
    core.apply_function = &apply_function;

    try make_environment();

    const args = try std.process.argsAlloc(Allocator);
    const arg_list = try MalType.new_list();
    if(1 < args.len) {
        for (args[2..]) |arg| {
            const new_mal = try MalType.new_string(arg, false);
            try arg_list.List.data.append(Allocator, new_mal);
        }
    }
    const argv_sym = try MalType.new_symbol("*ARGV*", true);
    try repl_environment.set(argv_sym, arg_list);
    argv_sym.decref();

    if(args.len > 1) {
        const run_cmd = try std.fmt.allocPrint(Allocator, "(load-file \"{s}\")", .{args[1]});
        try rep(false, run_cmd);
        return;
    }

    try do_print_header();

    while(try getline("user> ")) |line| {
        defer Allocator.free(line);
        rep(true, line) catch |err| {
            try stdout_file.writeAll("Error: ");
            try stdout_file.writeAll(@errorName(err));
            try stdout_file.writeAll("\n");
            if(get_error_data()) |mal| {
                defer mal.decref();
                try stdout_file.writeAll("MAL error object is: ");
                try PRINT(mal.*);
            }
        };
    }
}
