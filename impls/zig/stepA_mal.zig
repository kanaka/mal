const std = @import("std");
const warn = @import("std").debug.warn;

const reader = @import("reader.zig");
const printer = @import("printer.zig");
const getline = @import("readline.zig").getline;
const string_eql = @import("utils.zig").string_eql;
const string_copy = @import("utils.zig").string_copy;
const string_concat = @import("utils.zig").string_concat;
const apply_function_unsafe = @import("types.zig").apply_function;
const linked_list = @import("linked_list.zig");
const hash_map = @import("hmap.zig");
const core = @import("core.zig");
const error_string_repr = @import("error.zig").error_string_repr;

const CAllocator = @import("std").heap.c_allocator;
const AllocatorType = @import("std").mem.Allocator;
pub var Allocator: *AllocatorType = undefined;

const MalType = @import("types.zig").MalType;
const MalTypeValue = @import("types.zig").MalTypeValue;
const MalData = @import("types.zig").MalData;
const MalError = @import("error.zig").MalError;
const MalFuncData = @import("types.zig").MalFuncData;
const MalLinkedList = @import("linked_list.zig").MalLinkedList;
const Env = @import("env.zig").Env;

var repl_environment: *Env = undefined;

fn READ(a: []const u8) MalError!?*MalType {
    var read = try reader.read_str(a);
    var optional_mal = reader.read_form(&read);
    return optional_mal;
}

fn EVAL(mal_arg: *MalType, env_arg: *Env) MalError!*MalType {
    var mal = mal_arg;
    var env = env_arg;
    while(true) {
        mal = try macroexpand(mal, env);
        switch(mal.data) {
            .List => |ll| {
                if(ll.len == 0) {
                    env.delete();
                    return mal;
                }
                var first_mal = linked_list.first(&ll) orelse return MalError.ArgError;
                var symbol = switch(first_mal.data) {
                    .Generic => |symbol| symbol,
                    else => "",
                };
                if(string_eql(symbol, "def!")) {
                    return EVAL_def(mal, env, false);
                }
                else if(string_eql(symbol, "defmacro!")) {
                    return EVAL_def(mal, env, true);
                }
                else if(string_eql(symbol, "let*")) {
                    try EVAL_let(&mal, &env);
                    continue;
                }
                else if(string_eql(symbol, "do")) {
                    try EVAL_do(&mal, &env);
                    continue;
                }
                else if(string_eql(symbol, "if")) {
                    try EVAL_if(&mal, &env);
                    continue;
                }
                else if(string_eql(symbol, "fn*")) {
                    return EVAL_fn(mal, env);
                }
                else if(string_eql(symbol, "quote")) {
                    return EVAL_quote(mal, env);
                }
                else if(string_eql(symbol, "quasiquoteexpand")) {
                    env.delete();
                    (try mal.sequence_pop_first(Allocator)).delete(Allocator);
                    var second = try mal.sequence_pop_first(Allocator);
                    mal.delete(Allocator);
                    return try quasiquote(second);
                }
                else if(string_eql(symbol, "quasiquote")) {
                    (try mal.sequence_pop_first(Allocator)).delete(Allocator);
                    var second = try mal.sequence_pop_first(Allocator);
                    mal.delete(Allocator);
                    mal = try quasiquote(second);
                    continue;
                }
                else if(string_eql(symbol, "macroexpand")) {
                    (try mal.sequence_pop_first(Allocator)).delete(Allocator);
                    var second = try mal.sequence_pop_first(Allocator);
                    const expanded = macroexpand(second, env);
                    env.delete();
                    return expanded;
                }
                else if(string_eql(symbol, "try*")) {
                    return EVAL_try(mal, env);
                }
                else {
                    var new_list = try eval_ast(mal, try env.copy(Allocator));

                    if(MalTypeValue((try new_list.sequence_nth(0)).data) == MalTypeValue.Func) {
                        try do_user_func(try new_list.sequence_linked_list(), &mal, &env);
                        new_list.shallow_destroy(Allocator);
                        continue;
                    }
                    const res = try apply_function((try new_list.sequence_linked_list()).*);
                    new_list.delete(Allocator);
                    env.delete();
                    return res;
                }
            },
            else => {
                return eval_ast(mal, env);
            },
        }
    }
}

fn eval(a1: *MalType) MalError!*MalType {
    return EVAL(try a1.copy(Allocator), try repl_environment.copy(Allocator));
}

fn starts_with(ast: *MalType, sym: []const u8) bool {
    const ll = switch(ast.data) {
        .List => |l| l,
        else => return false,
    };
    if(ll.count() < 2) {
        return false;
    }
    const ss = switch(ll.at(0).data) {
        .Generic => |s| s,
        else => return false,
    };
    return string_eql(ss, sym);
}

fn is_macro_call(mal: *MalType, env: *Env) ?*MalType {
    const ll = switch(mal.data) {
        .List => |l| l,
        else => return null,
    };
    const first_node = linked_list.first(&ll) orelse return null;
    const symbol = switch(first_node.data) {
        .Generic => |s| s,
        else => return null,
    };
    const val = lookup(env, symbol, false) catch return null;
    const is_macro = switch(val.data) {
        .Func => |f| f.is_macro,
        else => false,
    };
    if(is_macro) {
        return val;
    }
    val.delete(Allocator);
    return null;
}

fn macroexpand(mal: *MalType, env: *Env) MalError!*MalType {
    var cur_mal = mal;
    var optional_macro = is_macro_call(cur_mal, env);
    while(optional_macro) |macro| {
        var new_list = (try cur_mal.sequence_linked_list()).*;

        if(new_list.count() > 0) {
            const first = try linked_list.pop_first(Allocator, &new_list);
            first.delete(Allocator);
        }
        try linked_list.prepend_mal(Allocator, &new_list, macro);
        var new_mal = try apply_function_unsafe(Allocator, new_list); 
        linked_list.destroy(Allocator, &new_list, false);        
        cur_mal.shallow_destroy(Allocator);
        cur_mal = new_mal;
        optional_macro = is_macro_call(cur_mal, env);
    }
    return cur_mal;
}

fn EVAL_def(mal: *MalType, env: *Env, macro: bool) MalError!*MalType {
    const first_arg = try mal.sequence_nth(1);
    const second_arg = try mal.sequence_nth(2);
    const second_arg_copy = try second_arg.copy(Allocator);
    const symbol_name = try first_arg.as_symbol();
    const new_value = try EVAL(second_arg_copy, try env.copy(Allocator));
    if(macro) {
        var func_data = switch(new_value.data) {
            .Func => |*f| f,
            else => return MalError.TypeError,
        };
        func_data.*.is_macro = true;
    }
    try env.set(symbol_name, new_value);
    mal.delete(Allocator);
    env.delete();
    return new_value.copy(Allocator);
}

fn EVAL_let(mal_ptr: **MalType, env_ptr: **Env) MalError!void {
    //TODO: make faster
    const mal = mal_ptr.*;
    const env = env_ptr.*;
    (try mal.sequence_pop_first(Allocator)).delete(Allocator);
    const binding_arg = try mal.sequence_pop_first(Allocator);
    const eval_arg = try mal.sequence_pop_first(Allocator);
    const new_env = try Env.new(Allocator, env);
    var binding_ll = switch(binding_arg.data) {
        .List => |l| l,
        .Vector => |v| v,
        else => return MalError.TypeError,
    };
    var iterator = binding_ll.iterator();
    var optional_node = iterator.next();
    while(optional_node) |node| {
        const key_mal = node;
        const key = try key_mal.as_symbol();
        const val_mal = iterator.next() orelse return MalError.ArgError;
        const evaled_mal = try EVAL(val_mal, try new_env.copy(Allocator));
        try new_env.set(key, evaled_mal);
        optional_node = iterator.next();
        key_mal.delete(Allocator);
    }
    
    linked_list.destroy(Allocator, &binding_ll, true);
    binding_arg.data = MalData{.Nil=undefined};
    binding_arg.delete(Allocator);
    mal.delete(Allocator);

    // We use eval_arg_copy, since we just deleted eval_arg above
    mal_ptr.* = eval_arg;
    env.delete();
    env_ptr.* = new_env;
}

fn EVAL_do(mal_ptr: **MalType, env_ptr: **Env) MalError!void {
    var mal = mal_ptr.*;
    var env = env_ptr.*;
    var ll = &mal.data.List;
    (try mal.sequence_pop_first(Allocator)).delete(Allocator);
    var last_mal = try mal.sequence_pop_last(Allocator);
    var evaled_mal = try eval_ast(mal, try env.copy(Allocator));
    evaled_mal.delete(Allocator);
    mal_ptr.* = last_mal;
}

fn EVAL_if(mal_ptr: **MalType, env_ptr: **Env) MalError!void {
    var mal = mal_ptr.*;
    var env = env_ptr.*;
    defer mal.delete(Allocator);
    const first_arg = try mal.sequence_nth(1);
    const first_arg_copy = try first_arg.copy(Allocator);
    const evaled = try EVAL(first_arg_copy, try env.copy(Allocator));
    const is_true = switch(evaled.data) {
        .False => false,
        .Nil => false,
        else => true,
    };
    evaled.delete(Allocator);
    if(is_true) {
        const second_arg = try mal.sequence_nth(2);
        mal_ptr.* = try second_arg.copy(Allocator);
        return;
    }
    if((try mal.sequence_length()) < 4) {
        mal_ptr.* = try MalType.new_nil(Allocator);
        return;
    }
    const third_arg = try mal.sequence_nth(3);
    const third_arg_copy = try third_arg.copy(Allocator);
    mal_ptr.* = third_arg_copy;
}

fn EVAL_fn(mal: *MalType, env: *Env) MalError!*MalType {
    defer mal.delete(Allocator);
    const arg_mal = try (try mal.sequence_nth(1)).copy(Allocator);
    const body_mal = try (try mal.sequence_nth(2)).copy(Allocator);
    const func_data = MalFuncData {
        .arg_list = arg_mal,
        .body = body_mal,
        .environment = env,
        .is_macro = false,
        .eval_func = &EVAL,
    };
    const new_func = try MalType.new_nil(Allocator);
    new_func.data = MalData{.Func = func_data};
    return new_func;
}

fn EVAL_quote(mal: *MalType, env: *Env) MalError!*MalType {
    defer mal.delete(Allocator);
    defer env.delete();
    (try mal.sequence_pop_first(Allocator)).delete(Allocator);
    return try mal.sequence_pop_first(Allocator);
}

fn EVAL_try(mal: *MalType, env: *Env) MalError!*MalType {
    defer mal.delete(Allocator);
    (try mal.sequence_pop_first(Allocator)).delete(Allocator);
    var mal_to_try = try mal.sequence_pop_first(Allocator);
    if(0 == try mal.sequence_length()) {
        return EVAL(mal_to_try, env);
    }
    var catch_mal = try mal.sequence_pop_first(Allocator);
    
    const evaled_mal = EVAL(mal_to_try, try env.copy(Allocator)) catch |err| {
        switch(err) {
            MalError.ThrownError => {
            },
            else => {
                const error_mal = try MalType.new_string(Allocator, error_string_repr(err));
                try env.set("__error", error_mal);
            }
        }
        // TODO: check that first element of catch is "catch*"
        (try catch_mal.sequence_pop_first(Allocator)).delete(Allocator);
        const err_symbol = try catch_mal.sequence_pop_first(Allocator);
        const err_body =try catch_mal.sequence_pop_first(Allocator);
        catch_mal.delete(Allocator);

        const err_val = try lookup(env, "__error", false);
        var new_env = try Env.new(Allocator, env);
        try new_env.set(try err_symbol.as_symbol(), err_val);
        err_symbol.delete(Allocator);
        const result = EVAL(err_body, try new_env.copy(Allocator));
        new_env.delete();
        env.delete();
        return result;
    };
    env.delete();
    return evaled_mal;
}

fn quasiquote(ast: *MalType) MalError!*MalType {
    const kind = MalTypeValue(ast.data);
    if(kind == MalTypeValue.Generic or kind == MalTypeValue.HashMap) {
        const new_list = try MalType.new_list_empty(Allocator);
        try new_list.sequence_append(Allocator, try MalType.new_generic(Allocator, "quote"));
        try new_list.sequence_append(Allocator, ast);
        return new_list;
    }

    if(kind != MalTypeValue.List and kind != MalTypeValue.Vector) {
        return ast;
    }

    defer ast.delete(Allocator);

    if(starts_with(ast, "unquote")) {
        (try ast.sequence_pop_first(Allocator)).delete(Allocator);
        return ast.sequence_pop_first(Allocator);
    }

    var result = try MalType.new_list_empty(Allocator);
    while(0 < (try ast.sequence_length())) {
        var elt = try ast.sequence_pop_last(Allocator);
        const new_list = try MalType.new_list_empty(Allocator);
        if(starts_with(elt, "splice-unquote")) {
            (try elt.sequence_pop_first(Allocator)).delete(Allocator);
            defer elt.delete(Allocator);
            try new_list.sequence_append(Allocator, try MalType.new_generic(Allocator, "concat"));
            try new_list.sequence_append(Allocator, try elt.sequence_pop_first(Allocator));
        } else {
            try new_list.sequence_append(Allocator, try MalType.new_generic(Allocator, "cons"));
            try new_list.sequence_append(Allocator, try quasiquote(elt));
        }
        try new_list.sequence_append(Allocator, result);
        result = new_list;
    }

    if(kind == MalTypeValue.Vector) {
        const new_list = try MalType.new_list_empty(Allocator);
        try new_list.sequence_append(Allocator, try MalType.new_generic(Allocator, "vec"));
        try new_list.sequence_append(Allocator, result);
        result = new_list;
    }
    return result;
}

fn PRINT(optional_mal: ?*MalType) MalError![] u8 {
    return printer.print_str(optional_mal);
}

fn rep(environment: *Env, input: [] const u8) MalError!?[] u8 {
    var read_input = (try READ(input)) orelse return null;
    var eval_input = try EVAL(read_input, try environment.copy(Allocator));
    var print_input = try PRINT(eval_input);
    eval_input.delete(Allocator);
    return print_input;
}

fn rep_and_print_errors(environment: *Env, input: [] const u8) ?[]u8 {
    return rep(environment, input) catch |err| {
        switch(err) {
            MalError.KeyError => { },
            MalError.OutOfBounds => {
                warn("Error: out of bounds\n");
            },
            MalError.ThrownError => {
                warn("Thrown error: ");
                const error_mal = lookup(environment, "__error", false)
                    catch {warn("<ERROR WHILE LOOKING UP ERROR>\n"); return null;};
                const warning = PRINT(error_mal)
                    catch {warn("<ERROR WHILE PRINTING ERROR>\n"); return null;};
                warn("{}\n", warning);
                error_mal.delete(Allocator);
                Allocator.free(warning);
            },
            MalError.ReaderUnmatchedParen => {
                warn("Error: expected closing paren, got EOF\n");
            },
            else => {
               warn("Error: {}\n", error_string_repr(err));
            },
        }
        return null;
    };
}

fn lookup(environment: *Env, symbol: []const u8, do_warn: bool) MalError!*MalType {
    var mal = environment.get(symbol) catch |err| {
        if(do_warn) {
            const s1 = string_concat(Allocator, "'", symbol) catch return MalError.SystemError;
            const s2 = string_concat(Allocator, s1, "' not found") catch return MalError.SystemError;
            defer Allocator.free(s1);
            defer Allocator.free(s2);
            _ = try throw(try MalType.new_string(Allocator, s2));
        }
        return MalError.KeyError;
    };
    var new_mal = try mal.copy(Allocator);
    return new_mal;
}

fn eval_ast(mal: *MalType, env: *Env) MalError!*MalType {
    defer env.delete();
    switch(mal.data) {
        .Generic => |symbol| {
            defer mal.delete(Allocator);
            return lookup(env, symbol, true);
        },
        .List => |*ll| {
            var new_ll = MalLinkedList.init(Allocator);
            var iterator = ll.iterator();
            while(iterator.next()) |next_mal| {
                const new_mal = try EVAL(next_mal, try env.copy(Allocator));
                try linked_list.append_mal(Allocator, &new_ll, new_mal);
            }
            linked_list.destroy(Allocator, ll, true);
            mal.shallow_destroy(Allocator);
            const ret_mal = MalType.new_list(Allocator, new_ll);
            return ret_mal;
        },
        .Vector => |*ll| {
            var new_ll = MalLinkedList.init(Allocator);
            var iterator = ll.iterator();
            while(iterator.next()) |next_mal| {
                const new_mal = try EVAL(next_mal, try env.copy(Allocator));
                try linked_list.append_mal(Allocator, &new_ll, new_mal);
            }
            linked_list.destroy(Allocator, ll, true);
            mal.shallow_destroy(Allocator);
            const ret_mal = MalType.new_vector(Allocator, new_ll);
            return ret_mal;
        },
        .HashMap => |hmap| {
            var new_hashmap = try MalType.new_hashmap(Allocator);
            var iterator = hmap.iterator();
            var optional_pair = iterator.next();
            while(true) {
                const pair = optional_pair orelse break;
                const key = pair.key;
                const value = pair.value;
                const evaled_value = try EVAL(value, try env.copy(Allocator));
                try new_hashmap.hashmap_insert(key, evaled_value);
                optional_pair = iterator.next();
            }
            hash_map.destroy(Allocator, hmap, true);
            mal.shallow_destroy(Allocator);
            return new_hashmap;
        },
        else => {
            return mal;
        }
    }
}

fn throw(a1: *MalType) MalError!*MalType {
    const error_mal = try a1.copy(Allocator);
    try repl_environment.set("__error", error_mal);
    return MalError.ThrownError;
}

fn make_environment() MalError!*Env {
    repl_environment = try Env.new(Allocator, null);
    var environment = try repl_environment.copy(Allocator);

   for(core.core_namespace) |pair| {
        const name = pair.name;
        const func_mal: *MalType = try MalType.new_nil(Allocator);
        func_mal.data = switch(pair.func) {
            core.CorePairType.Fn0 => |func| MalData{.Fn0 = func},
            core.CorePairType.Fn1 => |func| MalData{.Fn1 = func},
            core.CorePairType.Fn2 => |func| MalData{.Fn2 = func},
            core.CorePairType.Fn3 => |func| MalData{.Fn3 = func},
            core.CorePairType.Fn4 => |func| MalData{.Fn4 = func},
            core.CorePairType.FVar => |func| MalData{.FVar = func},
            else => return MalError.TypeError,
        };
        try environment.set(name, func_mal);
    }

    const eval_mal = try MalType.new_nil(Allocator);
    eval_mal.data = MalData{.Fn1 = &eval};
    try environment.set("eval", eval_mal);

    const throw_mal = try MalType.new_nil(Allocator);
    throw_mal.data = MalData{.Fn1 = &throw};
    try environment.set("throw", throw_mal);
    
    const def_not_string: [] const u8 =
        \\(def! not (fn* (a) (if a false true)))
    ;
    var optional_output = try rep(environment, def_not_string);
    if(optional_output) |output| {
        Allocator.free(output);
    }

    const load_file_string: [] const u8 =
        \\(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
    ;
    optional_output = try rep(environment, load_file_string);
    if(optional_output) |output| {
        Allocator.free(output);
    }

    const def_cond_macro_string: [] const u8 =
        \\(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))
    ;
    optional_output = try rep(environment, def_cond_macro_string);
    if(optional_output) |output| {
        Allocator.free(output);
    }

    try environment.set("*host-language*", try MalType.new_string(Allocator, "Zig"));

    return environment;
}

fn do_print_header(environment: *Env) MalError!void {   
    const welcome_msg_cmd: [] const u8 =
        \\(println (str "Mal [" *host-language* "]"))
    ;
    var optional_output = try rep(environment, welcome_msg_cmd);
    if(optional_output) |output| {
        Allocator.free(output);
    }
}

fn do_user_func(args: *MalLinkedList, mal_ptr: **MalType, env_ptr: **Env) MalError!void {
    const mal_func = try linked_list.pop_first(Allocator, args);
    const env = env_ptr.*;
    // First check if it is a user-defined Mal function
    if(MalTypeValue(mal_func.data) == MalTypeValue.Func) {
        const func_data = mal_func.data.Func;
        const args_ll = try func_data.arg_list.sequence_linked_list();
        const func_env = func_data.environment;
        var new_env = try Env.new(Allocator, func_env);
        func_env.delete();
        try new_env.set_list(args_ll.*, args.*);
        linked_list.destroy(Allocator, args, true);
        func_data.arg_list.delete(Allocator);
        mal_func.shallow_destroy(Allocator);
        mal_ptr.* = func_data.body;
        env.delete();
        env_ptr.* = new_env;
        return;
    }
    return MalError.TypeError;
}

fn apply_function(args: MalLinkedList) MalError!*MalType {

    const return_mal = apply_function_unsafe(Allocator, args) catch |err| {
        if(err == MalError.ReaderUnmatchedParen) {
            warn("Error: expected closing paren, got EOF\n");
        } else if(err == MalError.ReaderUnmatchedString) {
            warn("Error: expected closing string, got EOF\n");
        }
        return err;
    };
    return return_mal;
}

pub fn main() !void {
    const stdout_file = try std.io.getStdOut();
    Allocator = CAllocator;
    core.set_allocator(Allocator);
    
    var environment = try make_environment();

    const args = try std.process.argsAlloc(Allocator);
    var arg_list = try MalType.new_list_empty(Allocator);
    for(args) |arg,i| {
        if(i < 2) continue;
        const new_mal = try MalType.new_string(Allocator, arg);
        try arg_list.sequence_append(Allocator, new_mal);
    }
    try environment.set("*ARGV*", arg_list);
    
    if(args.len > 1) {
        const run_cmd = try string_concat(Allocator, try string_concat(Allocator, "(load-file \"", args[1]), "\")");
        var output = rep_and_print_errors(environment, run_cmd);
        return;
    }
    
    try do_print_header(repl_environment);
    
    while(true) {
        var line = (try getline(Allocator)) orelse break;
        var output = rep_and_print_errors(environment, line) orelse continue;
        try stdout_file.write(output);
        Allocator.free(output);
        Allocator.free(line);
        try stdout_file.write("\n");        
    }
}
