import std.stdio;
import std.string;
import readline;

string READ(string str)
{
    return str;
}

string EVAL(string ast)
{
    return ast;
}

string PRINT(string ast)
{
    return ast;
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
        writeln(rep(line));
    }
    writeln("");
}
