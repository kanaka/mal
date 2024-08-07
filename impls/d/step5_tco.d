module main;

import std.algorithm;
import std.array;
import std.range;
import std.stdio;
import std.string;
import env;
import mal_core;
import readline;
import reader;
import printer;
import types;

MalType READ(string str)
{
    return read_str(str);
}

MalType EVAL(MalType ast, Env env)
{
  for (;;)
  {
    if (auto dbgeval = env.get("DEBUG-EVAL"))
        if (dbgeval.is_truthy())
            writeln("EVAL: ", pr_str(ast));

    if (auto sym = cast(MalSymbol)ast)
    {
        if (auto val = env.get(sym.name))
            return val;
        else
            throw new Exception("'" ~ sym.name ~ "' not found");
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
    else if (auto ast_list = cast(MalList)ast)
    {
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
                return env.set(a1.name, EVAL(aste[2], env));

            case "let*":
                auto a1 = verify_cast!MalSequential(aste[1]);
                auto let_env = new Env(env);
                foreach (kv; chunks(a1.elements, 2))
                {
                    if (kv.length < 2) throw new Exception("let* requires even number of elements");
                    auto var_name = verify_cast!MalSymbol(kv[0]);
                    let_env.set(var_name.name, EVAL(kv[1], let_env));
                }
                ast = aste[2];
                env = let_env;
                continue; // TCO

            case "do":
                foreach (elt; aste[1..$-1]) {
                    EVAL(elt, env);
                }
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
                auto first = EVAL(aste[0], env);
                auto rest = array(aste[1..$].map!(e => EVAL(e, env)));
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
    else
    {
        return ast;
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

void main()
{
    auto repl_env = new Env(null);
    foreach (string sym_name, BuiltinStaticFuncType f; core_ns)
    {
        repl_env.set(sym_name, new MalBuiltinFunc(f, sym_name));
    }

    // core.mal: defined using the language itself
    re("(def! not (fn* (a) (if a false true)))", repl_env);

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
