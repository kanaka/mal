// @import types/boxed/*.ck
// @import types/MalObject.ck
// @import types/mal/*.ck
// @import util/*.ck
// @import reader.ck
// @import printer.ck
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
            return MalError.create(Status.SYMBOL_NOT_FOUND, symbol);
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

fun string rep(string input)
{
    READ(input) @=> MalObject m;

    if( m.type == "error" )
    {
        return Status.toMessage(m$MalError);
    }

    EVAL(m, repl_env) @=> MalObject result;
    if( result.type == "error" )
    {
        return Status.toMessage(result$MalError);
    }

    return PRINT(result);
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
