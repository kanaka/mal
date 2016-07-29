// @import types/boxed/*.ck
// @import types/MalObject.ck
// @import types/mal/*.ck
// @import util/*.ck
// @import reader.ck
// @import printer.ck
// @import env.ck
// @import types/MalSubr.ck
// @import types/subr/*.ck

fun MalObject READ(string input)
{
    return Reader.read_str(input);
}

fun MalObject EVAL(MalObject m, Env env)
{
    if( m.type == "list" )
    {
        if( (m$MalList).value().size() == 0 )
        {
            return m;
        }

        (m$MalList).value() @=> MalObject ast[];
        (ast[0]$MalSymbol).value() => string a0;

        if( a0 == "def!" )
        {
            (ast[1]$MalSymbol).value() => string a1;

            EVAL(ast[2], env) @=> MalObject value;
            if( value.type == "error" )
            {
                return value;
            }

            env.set(a1, value);
            return value;
        }
        else if( a0 == "let*" )
        {
            Env.create(env) @=> Env let_env;
            Util.sequenceToMalObjectArray(ast[1]) @=> MalObject bindings[];

            for( 0 => int i; i < bindings.size(); 2 +=> i)
            {
                (bindings[i]$MalSymbol).value() => string symbol;
                EVAL(bindings[i+1], let_env) @=> MalObject value;

                if( value.type == "error" )
                {
                    return value;
                }

                let_env.set(symbol, value);
            }

            return EVAL(ast[2], let_env);
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
        eval_ast(m, env) @=> MalObject result;
        return result;
    }
}

fun MalObject eval_ast(MalObject m, Env env)
{
    m.type => string type;

    if( type == "symbol" )
    {
        (m$MalSymbol).value() => string symbol;
        return env.get(symbol);
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

Env.create(null) @=> Env repl_env;
repl_env.set("+", new MalAdd);
repl_env.set("-", new MalSub);
repl_env.set("*", new MalMul);
repl_env.set("/", new MalDiv);

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
