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
// @import core.ck

fun MalObject READ(string input)
{
    return Reader.read_str(input);
}

fun MalObject EVAL(MalObject m, Env env)
{
    while( true )
    {
        if( m.type != "list" )
        {
            return eval_ast(m, env);
        }

        if( (m$MalList).value().size() == 0 )
        {
            return m;
        }

        (m$MalList).value() @=> MalObject ast[];

        if( ast[0].type == "symbol" )
        {
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

                let_env @=> env;
                ast[2] @=> m;
                continue; // TCO
            }
            else if( a0 == "do" )
            {
                MalObject.slice(ast, 1, ast.size()-1) @=> MalObject forms[];
                eval_ast(MalList.create(forms), env) @=> MalObject value;

                if( value.type == "error" )
                {
                    return value;
                }

                // HACK: this assumes do gets at least one argument...
                ast[ast.size()-1] @=> m;
                continue; // TCO
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
                    ast[2] @=> m;
                    continue; // TCO
                }
                else
                {
                    if( ast.size() < 4 )
                    {
                        return Constants.NIL;
                    }
                    else
                    {
                        ast[3] @=> m;
                        continue; // TCO
                    }
                }
            }
            else if( a0 == "fn*" )
            {
                (ast[1]$MalList).value() @=> MalObject arg_values[];
                string args[arg_values.size()];

                for( 0 => int i; i < arg_values.size(); i++ )
                {
                    (arg_values[i]$MalSymbol).value() => args[i];
                }

                ast[2] @=> MalObject _ast;

                return Func.create(env, args, _ast);
            }
        }

        eval_ast(m, env) @=> MalObject result;
        if( result.type == "error" )
        {
            return result;
        }

        (result$MalList).value() @=> MalObject values[];
        values[0].type => string type;
        MalObject.slice(values, 1) @=> MalObject args[];

        if( type == "subr" )
        {
            values[0]$MalSubr @=> MalSubr subr;
            return subr.call(args);
        }
        else // type == "func"
        {
            values[0]$Func @=> Func func;
            Env.create(func.env, func.args, args) @=> Env eval_env;
            eval_env @=> env;
            func.ast @=> m;
            continue; // TCO
        }
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
for( 0 => int i; i < Core.names.size(); i++ )
{
    Core.names[i] => string name;
    repl_env.set(name, Core.ns[name]);
}

// HACK, HACK, HACK
class MalEval extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject m;
        return EVAL(args[0], repl_env);
    }

    fun MalObject apply(MalObject f, MalObject args[])
    {
        if( f.type == "subr" )
        {
            return (f$MalSubr).call(args);
        }
        else // f.type == "func"
        {
            f$Func @=> Func func;
            Env.create(func.env, func.args, args) @=> Env eval_env;
            return EVAL(func.ast, eval_env);
        }
    }
}

new MalEval @=> MalEval eval;
repl_env.set("eval", new MalEval);
eval @=> (repl_env.get("swap!")$MalSubr).eval;

fun MalObject[] MalArgv(string args[])
{
    MalObject values[0];

    for( 1 => int i; i < args.size(); i++ )
    {
        values << MalString.create(args[i]);
    }

    return values;
}

// NOTE: normally I'd use \0, but strings are null-terminated...
String.split(Std.getenv("CHUCK_ARGS"), "\a") @=> string args[];
repl_env.set("*ARGV*", MalList.create(MalArgv(args)));

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

rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");

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

if( args.size() > 0 )
{
    args[0] => string filename;
    rep("(load-file \"" + filename + "\")");
}
else
{
    main();
}
