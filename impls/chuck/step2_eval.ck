// @import readline.ck
// @import types/MalObject.ck
// @import types/mal/MalAtom.ck
// @import types/mal/MalString.ck
// @import types/mal/MalError.ck
// @import types/mal/MalNil.ck
// @import types/mal/MalFalse.ck
// @import types/mal/MalTrue.ck
// @import types/mal/MalInt.ck
// @import types/mal/MalSymbol.ck
// @import types/mal/MalKeyword.ck
// @import types/mal/MalList.ck
// @import types/mal/MalVector.ck
// @import types/mal/MalHashMap.ck
// @import util/*.ck
// @import reader.ck
// @import printer.ck
// @import env.ck
// @import func.ck
// @import types/MalSubr.ck
// @import types/subr/*.ck

fun MalObject READ(string input)
{
    return Reader.read_str(input);
}

fun MalObject EVAL(MalObject m, MalSubr env[])
{

    //  Util.println("EVAL: " + Printer.pr_str(m, true));

    if( m.type == "symbol" )
    {
        m.stringValue => string symbol;
        env[symbol] @=> MalSubr subr;

        if( subr == null )
        {
            return MalError.create("'" + symbol + "' not found");
        }
        else
        {
            return subr;
        }
    }
    else if( m.type == "vector" )
    {
        m.malObjectValues() @=> MalObject values[];
        MalObject results[values.size()];
        for( 0 => int i; i < values.size(); i++ )
        {
            EVAL(values[i], env) @=> MalObject result;
            if( result.type == "error" )
            {
                return result;
            }
            result @=> results[i];
        }
        return MalVector.create(results);
    }
    else if( m.type == "hashmap" )
    {
        m.malObjectValues() @=> MalObject values[];
        MalObject results[values.size()];
        for( 0 => int i; i < values.size(); i++ )
        {
            if( i % 2 == 0 )
            {
                values[i] @=> results[i];
            }
            else
            {
                EVAL(values[i], env) @=> results[i];
            }
        }
        return MalHashMap.create(results);
    }
    else if( m.type == "list" )
    {
        m.malObjectValues() @=> MalObject ast[];

        if( ast.size() == 0 )
        {
            return m;
        }

        EVAL(ast[0], env) @=> MalObject first;
        if( first.type == "error" )
        {
            return first;
        }

        MalObject args[ast.size() - 1];
        for( 0 => int i; i < args.size(); i++ )
        {
            EVAL(ast[i + 1], env) @=> MalObject result;
            if( result.type == "error" )
            {
                return result;
            }
            result @=> args[i];
        }
        if( first.type == "subr" )
        {
            first$MalSubr @=> MalSubr subr;
            return subr.call(args);
        }
        else
        {
            Util.panic("Programmer error: cannot apply");
            return null;
        }
    }
    else
    {
        return m;
    }
}

fun string PRINT(MalObject m)
{
    return Printer.pr_str(m, true);
}

MalSubr repl_env[0];
new MalAdd @=> repl_env["+"];
new MalSub @=> repl_env["-"];
new MalMul @=> repl_env["*"];
new MalDiv @=> repl_env["/"];

fun string errorMessage(MalObject m)
{
    return "exception: " + String.repr(m.malObjectValue().stringValue);
}

fun string rep(string input)
{
    READ(input) @=> MalObject m;

    if( m.type == "error" )
    {
        return errorMessage(m);
    }

    EVAL(m, repl_env) @=> MalObject result;
    if( result.type == "error" )
    {
        return errorMessage(result);
    }

    return PRINT(result);
}

fun void main()
{
    int done;

    while( !done )
    {
        Readline.readline("user> ") => string input;

        if( input != null )
        {
            rep(input) => string output;

            if( output == "exception: \"empty input\"" )
            {
                // proceed immediately with prompt
            }
            else
            {
                Util.println(output);
            }
        }
        else
        {
            true => done;
        }
    }
}

main();
