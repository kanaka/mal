module main;

import std.algorithm;
import std.array;
import std.range;
import std.stdio;
import std.string;
import std.c.process;
import env;
import mal_core;
import readline;
import reader;
import printer;
import types;

bool is_pair(MalType ast)
{
    auto lst = cast(MalSequential) ast;
    if (lst is null) return false;
    return lst.elements.length > 0;
}

MalType quasiquote(MalType ast)
{
    if (!is_pair(ast))
    {
        return new MalList([sym_quote, ast]);
    }
    auto ast_seq = verify_cast!MalSequential(ast);
    auto aste = ast_seq.elements;
    if (aste[0] == sym_unquote)
    {
        return aste[1];
    }

    if (is_pair(aste[0]))
    {
        auto ast0_seq = verify_cast!MalSequential(aste[0]);
        if (ast0_seq.elements[0] == sym_splice_unquote)
        {
            return new MalList([new MalSymbol("concat"), ast0_seq.elements[1], quasiquote(new MalList(aste[1..$]))]);
        }
    }

    return new MalList([new MalSymbol("cons"), quasiquote(aste[0]), quasiquote(new MalList(aste[1..$]))]);
}

MalType READ(string str)
{
    return read_str(str);
}

MalType eval_ast(MalType ast, Env env)
{
    if (typeid(ast) == typeid(MalSymbol))
    {
        auto sym = verify_cast!MalSymbol(ast);
        return env.get(sym);
    }
    else if (typeid(ast) == typeid(MalList))
    {
        auto lst = verify_cast!MalList(ast);
        auto el = array(lst.elements.map!(e => EVAL(e, env)));
        return new MalList(el);
    }
    else if (typeid(ast) == typeid(MalVector))
    {
        auto lst = verify_cast!MalVector(ast);
        auto el = array(lst.elements.map!(e => EVAL(e, env)));
        return new MalVector(el);
    }
    else if (typeid(ast) == typeid(MalHashmap))
    {
        auto hm = verify_cast!MalHashmap(ast);
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

        auto aste = ast_list.elements;
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

            case "quasiquote":
                ast = quasiquote(aste[1]);
                continue; // TCO

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
                if (typeid(first) == typeid(MalFunc))
                {
                    auto funcobj = verify_cast!MalFunc(first);
                    auto callenv = new Env(funcobj.def_env, funcobj.arg_names, rest);
                    ast = funcobj.func_body;
                    env = callenv;
                    continue; // TCO
                }
                else if (typeid(first) == typeid(MalBuiltinFunc))
                {
                    auto builtinfuncobj = verify_cast!MalBuiltinFunc(first);
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
    re("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", repl_env);

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
            std.c.process.exit(1);
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
