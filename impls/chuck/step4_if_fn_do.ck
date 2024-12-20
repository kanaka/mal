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
// @import core.ck

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

    if( m.type == "symbol" )
    {
        return env.get(m.stringValue);
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
        else if( ast[0].type == "symbol" )
        {
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
            else if( a0 == "do" )
            {
              MalObject value;
              for( 1 => int i; i < ast.size(); i++ )
              {
                EVAL(ast[i], env) @=> value;
                if( value.type == "error" )
                {
                    return value;
                }
              }
              return value;
            }
            else if( a0 == "if" )
            {
                EVAL(ast[1], env) @=> MalObject condition;

                if( condition.type == "error" )
                {
                    return condition;
                }

                if( !(condition.type == "nil") && !(condition.type == "false") )
                {
                    return EVAL(ast[2], env);
                }
                else
                {
                    if( ast.size() < 4 )
                    {
                        return Constants.NIL;
                    }
                    else
                    {
                        return EVAL(ast[3], env);
                    }
                }
            }
            else if( a0 == "fn*" )
            {
                ast[1].malObjectValues() @=> MalObject arg_values[];
                string args[arg_values.size()];

                for( 0 => int i; i < arg_values.size(); i++ )
                {
                    arg_values[i].stringValue => args[i];
                }

                ast[2] @=> MalObject _ast;

                return Func.create(env, args, _ast);
            }
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
        else if( first.type == "func" )
        {
            first$Func @=> Func func;
            Env.create(func.env, func.args, args) @=> Env eval_env;
            return EVAL(func.ast, eval_env);
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

Env.create(null) @=> Env repl_env;
for( 0 => int i; i < Core.names.size(); i++ )
{
    Core.names[i] => string name;
    repl_env.set(name, Core.ns[name]);
}

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

rep("(def! not (fn* (a) (if a false true)))");

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
