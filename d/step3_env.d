module main;

import std.algorithm;
import std.array;
import std.range;
import std.stdio;
import std.string;
import env;
import readline;
import reader;
import printer;
import types;

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
    MalList ast_list = cast(MalList) ast;
    if (ast_list is null)
    {
        return eval_ast(ast, env);
    }

    auto a0_sym = verify_cast!MalSymbol(ast_list.elements[0]);
    switch (a0_sym.name)
    {
        case "def!":
            auto a1 = verify_cast!MalSymbol(ast_list.elements[1]);
            return env.set(a1, EVAL(ast_list.elements[2], env));

        case "let*":
            auto a1 = verify_cast!MalSequential(ast_list.elements[1]);
            auto let_env = new Env(env);
            foreach (kv; chunks(a1.elements, 2))
            {
                if (kv.length < 2) throw new Exception("let* requires even number of elements");
                auto var_name = verify_cast!MalSymbol(kv[0]);
                let_env.set(var_name, EVAL(kv[1], let_env));
            }
            return EVAL(ast_list.elements[2], let_env);

        default:
            auto el = verify_cast!MalList(eval_ast(ast_list, env));
            auto fobj = verify_cast!MalBuiltinFunc(el.elements[0]);
            auto args = el.elements[1..$];
            return fobj.fn(args);
    }
}

string PRINT(MalType ast)
{
    return pr_str(ast);
}

string rep(string str, Env env)
{
    return PRINT(EVAL(READ(str), env));
}

static MalType mal_add(MalType[] a ...)
{
     verify_args_count(a, 2);
     MalInteger i0 = verify_cast!MalInteger(a[0]);
     MalInteger i1 = verify_cast!MalInteger(a[1]);
     return new MalInteger(i0.val + i1.val);
}

static MalType mal_sub(MalType[] a ...)
{
     verify_args_count(a, 2);
     MalInteger i0 = verify_cast!MalInteger(a[0]);
     MalInteger i1 = verify_cast!MalInteger(a[1]);
     return new MalInteger(i0.val - i1.val);
}

static MalType mal_mul(MalType[] a ...)
{
     verify_args_count(a, 2);
     MalInteger i0 = verify_cast!MalInteger(a[0]);
     MalInteger i1 = verify_cast!MalInteger(a[1]);
     return new MalInteger(i0.val * i1.val);
}

static MalType mal_div(MalType[] a ...)
{
     verify_args_count(a, 2);
     MalInteger i0 = verify_cast!MalInteger(a[0]);
     MalInteger i1 = verify_cast!MalInteger(a[1]);
     return new MalInteger(i0.val / i1.val);
}

void main()
{
    auto repl_env = new Env(null);
    repl_env.set(new MalSymbol("+"), new MalBuiltinFunc(&mal_add, "+"));
    repl_env.set(new MalSymbol("-"), new MalBuiltinFunc(&mal_sub, "-"));
    repl_env.set(new MalSymbol("*"), new MalBuiltinFunc(&mal_mul, "*"));
    repl_env.set(new MalSymbol("/"), new MalBuiltinFunc(&mal_div, "/"));

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
