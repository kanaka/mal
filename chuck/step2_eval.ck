// @import readline.ck
// @import types/boxed/*.ck
// @import types/MalObject.ck
// @import types/mal/MalAtom.ck
// @import types/mal/MalError.ck
// @import types/mal/MalNil.ck
// @import types/mal/MalFalse.ck
// @import types/mal/MalTrue.ck
// @import types/mal/MalInt.ck
// @import types/mal/MalString.ck
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
    if( m.type == "list" )
    {
        if( (m$MalList).value().size() == 0 )
        {
            return m;
        }

        eval_ast(m, env) @=> MalObject result;
        if( result.type == "error" )
        {
            return result;
        }

        (result$MalList).value() @=> MalObject values[];
        values[0]$MalSubr @=> MalSubr subr;
        MalObject.slice(values, 1) @=> MalObject args[];

        return subr.call(args);
    }
    else
    {
        return eval_ast(m, env);
    }
}

fun MalObject eval_ast(MalObject m, MalSubr env[])
{
    m.type => string type;

    if( type == "symbol" )
    {
        (m$MalSymbol).value() => string symbol;
        env[symbol] @=> MalSubr subr;

        if( subr == null )
        {
            return MalError.create(MalString.create("'" + symbol + "' not found"));
        }
        else
        {
            return subr;
        }
    }
    else if( type == "list" || type == "vector" || type == "hashmap" )
    {
        (m$MalList).value() @=> MalObject values[];
        MalObject results[values.size()];

        if( type != "hashmap" )
        {
            for( 0 => int i; i < values.size(); i++ )
            {
                EVAL(values[i], env) @=> MalObject result;

                if( result.type == "error" )
                {
                    return result;
                }

                result @=> results[i];
            }
        }
        else
        {
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
        }

        if( type == "list" )
        {
            return MalList.create(results);
        }
        else if( type == "vector" )
        {
            return MalVector.create(results);
        }
        else if( type == "hashmap" )
        {
            return MalHashMap.create(results);
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
    (m$MalError).value() @=> MalObject value;
    return "exception: " + Printer.pr_str(value, true);
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

            if( output == "empty input" )
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
