import std.algorithm;
import std.array;
import std.stdio;
import std.string;
import readline;
import reader;
import printer;
import types;

alias Env = MalType[string];

MalType READ(string str)
{
    return read_str(str);
}

MalType EVAL(MalType ast, Env env)
{
    if (auto dbgeval = ("DEBUG-EVAL" in env))
        if (dbgeval.is_truthy())
            writeln("EVAL: ", pr_str(ast));

    if (auto sym = cast(MalSymbol)ast)
    {
        auto v = (sym.name in env);
        if (v is null) throw new Exception("'" ~ sym.name ~ "' not found");
        return *v;
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
  // todo: indent right
  else if (auto ast_list = cast(MalList)ast)
  {
    if (ast_list.elements.length == 0)
    {
        return ast;
    }
    auto fobj = verify_cast!MalBuiltinFunc(EVAL(ast_list.elements[0], env));
    auto args = array(ast_list.elements[1..$].map!(e => EVAL(e, env)));
    return fobj.fn(args);
  }
  else
  {
        return ast;
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
    Env repl_env;
    repl_env["+"] = new MalBuiltinFunc(&mal_add, "+");
    repl_env["-"] = new MalBuiltinFunc(&mal_sub, "-");
    repl_env["*"] = new MalBuiltinFunc(&mal_mul, "*");
    repl_env["/"] = new MalBuiltinFunc(&mal_div, "/");

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
