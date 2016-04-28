// @import types/boxed/String.ck
// @import types/boxed/Int.ck

// @import types/mal/MalObject.ck
// @import types/mal/MalError.ck

// @import types/mal/MalAtom.ck

// @import types/mal/MalTrue.ck
// @import types/mal/MalFalse.ck
// @import types/mal/MalNil.ck

// @import types/mal/MalInt.ck
// @import types/mal/MalString.ck
// @import types/mal/MalSymbol.ck
// @import types/mal/MalKeyword.ck

// @import types/mal/MalList.ck
// @import types/mal/MalVector.ck
// @import types/mal/MalHashMap.ck

// @import util/Status.ck

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
