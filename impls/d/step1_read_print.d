import std.stdio;
import std.string;
import readline;
import reader;
import printer;
import types;

MalType READ(string str)
{
    return read_str(str);
}

MalType EVAL(MalType ast)
{
    return ast;
}

string PRINT(MalType ast)
{
    return pr_str(ast);
}

string rep(string str)
{
    return PRINT(EVAL(READ(str)));
}

void main()
{
    for (;;)
    {
        string line = _readline("user> ");
        if (line is null) break;
        if (line.length == 0) continue;
        try
        {
            writeln(rep(line));
        }
        catch (Exception e)
        {
            writeln("Error: ", e.msg);
        }
    }
    writeln("");
}
