// @import types/boxed/*.ck
// @import types/MalObject.ck
// @import types/mal/*.ck
// @import util/*.ck
// @import reader.ck
// @import printer.ck

fun MalObject READ(string input)
{
    return Reader.read_str(input);
}

fun MalObject EVAL(MalObject m)
{
    return m;
}

fun string PRINT(MalObject m)
{
    return Printer.pr_str(m, true);
}

fun string rep(string input)
{
    READ(input) @=> MalObject m;

    if( m.type == "error" )
    {
        return Status.toMessage(m$MalError);
    }
    else
    {
        return PRINT(EVAL(m));
    }
}

fun void main()
{
    ConsoleInput stdin;
    string input;

    while( true )
    {
        stdin.prompt("user>") => now;
        stdin.getLine() => input;
        rep(input) => string output;

        if( output == "empty input" )
        {
            // proceed immediately with prompt
        }
        else
        {
            chout <= output + "\n";
        }
    }
}

main();
