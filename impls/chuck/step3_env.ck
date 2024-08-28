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

fun MalObject EVAL(MalObject m, Env env)
{
    env.find("DEBUG-EVAL") @=> MalObject debugEval;
    if( debugEval != null && (debugEval.type != "false" &&
                              debugEval.type != "nil" ) )
    {
        Util.println("EVAL: " + Printer.pr_str(m, true));
    }

    if( m.type == "list" )
    {
        if( m.objects.size() == 0 )
        {
            return m;
        }

        m.malObjectValues() @=> MalObject ast[];
        ast[0].stringValue => string a0;

        if( a0 == "def!" )
        {
            ast[1].stringValue => string a1;

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
            ast[1].malObjectValues() @=> MalObject bindings[];

            for( 0 => int i; i < bindings.size(); 2 +=> i)
            {
                bindings[i].stringValue => string symbol;
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

        result.malObjectValues() @=> MalObject values[];
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
        return env.get(m.stringValue);
    }
    else if( type == "list" || type == "vector" || type == "hashmap" )
    {
        m.malObjectValues() @=> MalObject values[];
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
        else
        {
            Util.panic("Programmer error (exhaustive match)");
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

Env.create(null) @=> Env repl_env;
repl_env.set("+", new MalAdd);
repl_env.set("-", new MalSub);
repl_env.set("*", new MalMul);
repl_env.set("/", new MalDiv);

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
