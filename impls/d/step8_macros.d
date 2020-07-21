module main;

import std.algorithm;
import std.array;
import std.range;
import std.stdio;
import std.string;
import core.stdc.stdlib;
import env;
import mal_core;
import readline;
import reader;
import printer;
import types;

bool starts_with(MalType ast, MalSymbol sym)
{
    auto lst = cast(MalList) ast;
    if (lst is null) return false;
    auto lste = lst.elements;
    return lste.length > 0 && lste[0] == sym;
}

MalType quasiquote(MalType ast)
{
    if (cast(MalSymbol)ast || cast(MalHashmap)ast)
        return new MalList([sym_quote, ast]);

    auto ast_seq = cast(MalSequential) ast;
    if (ast_seq is null)
        return ast;

    auto aste = ast_seq.elements;
    if (starts_with(ast, sym_unquote))
        return aste[1];

    MalType res = new MalList([]);;
    foreach_reverse (elt; ast_seq.elements)
        if (starts_with(elt, sym_splice_unquote))
            res = new MalList([new MalSymbol("concat"), (cast(MalList) elt).elements[1], res]);
        else
            res = new MalList([new MalSymbol("cons"), quasiquote(elt), res]);
    if (cast(MalVector) ast)
        res = new MalList([new MalSymbol("vec"), res]);
    return res;
}

bool is_macro_call(MalType ast, Env env)
{
    auto lst = cast(MalList) ast;
    if (lst is null) return false;
    if (lst.elements.length == 0) return false;
    auto sym0 = cast(MalSymbol) lst.elements[0];
    if (sym0 is null) return false;
    if (env.find(sym0) is null) return false;
    auto val = env.get(sym0);
    auto val_func = cast(MalFunc) val;
    if (val_func is null) return false;
    return val_func.is_macro;
}

MalType macroexpand(MalType ast, Env env)
{
    while (is_macro_call(ast, env))
    {
        auto ast_list = verify_cast!MalList(ast);
        auto sym0 = verify_cast!MalSymbol(ast_list.elements[0]);
        auto macrofunc = verify_cast!MalFunc(env.get(sym0));
        auto rest = ast_list.elements[1..$];
        auto callenv = new Env(macrofunc.def_env, macrofunc.arg_names, rest);
        ast = EVAL(macrofunc.func_body, callenv);
    }
    return ast;
}

MalType READ(string str)
{
    return read_str(str);
}

MalType eval_ast(MalType ast, Env env)
{
    if (auto sym = cast(MalSymbol)ast)
    {
        return env.get(sym);
    }
    else if (auto lst = cast(MalList)ast)
    {
        auto el = array(lst.elements.map!(e => EVAL(e, env)));
        return new MalList(el);
    }
    else if (auto lst = cast(MalVector)ast)
    {
        auto el = array(lst.elements.map!(e => EVAL(e, env)));
        return new MalVector(el);
    }
    else if (auto hm = cast(MalHashmap)ast)
    {
        typeof(hm.data) new_data;
        foreach (string k, MalType v; hm.data)
        {
            new_data[k] = EVAL(v, env);
        }
        return new MalHashmap(new_data);
    }
    else
    {
        return ast;
    }
}

MalType EVAL(MalType ast, Env env)
{
    for (;;)
    {
        MalList ast_list = cast(MalList) ast;
        if (ast_list is null)
        {
            return eval_ast(ast, env);
        }

        ast = macroexpand(ast, env);
        ast_list = cast(MalList) ast;
        if (ast_list is null)
        {
            return eval_ast(ast, env);
        }

        auto aste = ast_list.elements;
        if (aste.length == 0)
        {
            return ast;
        }
        auto a0_sym = cast(MalSymbol) aste[0];
        auto sym_name = a0_sym is null ? "" : a0_sym.name;
        switch (sym_name)
        {
            case "def!":
                auto a1 = verify_cast!MalSymbol(aste[1]);
                return env.set(a1, EVAL(aste[2], env));

            case "let*":
                auto a1 = verify_cast!MalSequential(aste[1]);
                auto let_env = new Env(env);
                foreach (kv; chunks(a1.elements, 2))
                {
                    if (kv.length < 2) throw new Exception("let* requires even number of elements");
                    auto var_name = verify_cast!MalSymbol(kv[0]);
                    let_env.set(var_name, EVAL(kv[1], let_env));
                }
                ast = aste[2];
                env = let_env;
                continue; // TCO

            case "quote":
                return aste[1];

            case "quasiquoteexpand":
                return quasiquote(aste[1]);

            case "quasiquote":
                ast = quasiquote(aste[1]);
                continue; // TCO

            case "defmacro!":
                auto a1 = verify_cast!MalSymbol(aste[1]);
                auto mac = verify_cast!MalFunc(EVAL(aste[2], env));
                mac.is_macro = true;
                return env.set(a1, mac);

            case "macroexpand":
                return macroexpand(aste[1], env);

            case "do":
                auto all_but_last = new MalList(aste[1..$-1]);
                eval_ast(all_but_last, env);
                ast = aste[$-1];
                continue; // TCO

            case "if":
                auto cond = EVAL(aste[1], env);
                if (cond.is_truthy())
                {
                    ast = aste[2];
                    continue; // TCO
                }
                else
                    if (aste.length > 3)
                    {
                        ast = aste[3];
                        continue; // TCO
                    }
                    else
                    {
                        return mal_nil;
                    }

            case "fn*":
                auto args_list = verify_cast!MalSequential(aste[1]);
                return new MalFunc(args_list.elements, aste[2], env);

            default:
                auto el = verify_cast!MalList(eval_ast(ast, env));
                if (el.elements.length == 0)
                {
                    throw new Exception("Expected a non-empty list");
                }
                auto first = el.elements[0];
                auto rest = el.elements[1..$];
                if (auto funcobj = cast(MalFunc)first)
                {
                    auto callenv = new Env(funcobj.def_env, funcobj.arg_names, rest);
                    ast = funcobj.func_body;
                    env = callenv;
                    continue; // TCO
                }
                else if (auto builtinfuncobj = cast(MalBuiltinFunc)first)
                {
                    return builtinfuncobj.fn(rest);
                }
                else
                {
                    throw new Exception("Expected a function");
                }
        }
    }
}

string PRINT(MalType ast)
{
    return pr_str(ast);
}

MalType re(string str, Env env)
{
    return EVAL(READ(str), env);
}

string rep(string str, Env env)
{
    return PRINT(re(str, env));
}

static MalList create_argv_list(string[] args)
{
    if (args.length <= 2) return new MalList([]);
    return new MalList(array(args[2..$].map!(s => cast(MalType)(new MalString(s)))));
}

void main(string[] args)
{
    Env repl_env = new Env(null);
    foreach (string sym_name, BuiltinStaticFuncType f; core_ns)
    {
        repl_env.set(new MalSymbol(sym_name), new MalBuiltinFunc(f, sym_name));
    }

    BuiltinFuncType eval_func = (a ...) {
        verify_args_count(a, 1);
        return EVAL(a[0], repl_env);
    };
    repl_env.set(new MalSymbol("eval"), new MalBuiltinFunc(eval_func, "eval"));
    repl_env.set(new MalSymbol("*ARGV*"), create_argv_list(args));

    // core.mal: defined using the language itself
    re("(def! not (fn* (a) (if a false true)))", repl_env);
    re("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env);
    re("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env);

    if (args.length > 1)
    {
        try
        {
            rep("(load-file \"" ~ args[1] ~ "\")", repl_env);
            return;
        }
        catch (Exception e)
        {
            writeln("Error: ", e.msg);
            exit(1);
        }
    }

    for (;;)
    {
        string line = _readline("user> ");
        if (line is null) break;
        if (line.length == 0) continue;
        try
        {
            writeln(rep(line, repl_env));
        }
        catch (Exception e)
        {
            writeln("Error: ", e.msg);
        }
    }
    writeln("");
}
