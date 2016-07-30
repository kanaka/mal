// @import types/boxed/*.ck
// @import types/MalObject.ck
// @import types/mal/*.ck
// @import util/*.ck
// @import reader.ck
// @import printer.ck
// @import env.ck
// @import types/MalSubr.ck
// @import types/subr/*.ck
// @import core.ck
// @import func.ck

fun MalObject READ(string input)
{
    return Reader.read_str(input);
}

fun int isPair(MalObject m)
{
    if( (m.type == "list" || m.type == "vector") &&
        Util.sequenceToMalObjectArray(m).size() > 0 )
    {
        return true;
    }
    else
    {
        return false;
    }
}

fun MalObject quasiquote(MalObject ast)
{
    if( !isPair(ast) )
    {
        return MalList.create([MalSymbol.create("quote"), ast]);
    }

    Util.sequenceToMalObjectArray(ast) @=> MalObject a[];
    a[0] @=> MalObject a0;

    if( a0.type == "symbol" && (a0$MalSymbol).value() == "unquote" )
    {
        return a[1];
    }

    if( isPair(a0) )
    {
        Util.sequenceToMalObjectArray(a0) @=> MalObject a0_[];
        a0_[0] @=> MalObject a0_0;

        if( a0_0.type == "symbol" && (a0_0$MalSymbol).value() == "splice-unquote" )
        {
            return MalList.create(
                [MalSymbol.create("concat"), a0_[1],
                 quasiquote(MalList.create(MalObject.slice(a, 1)))]);
        }
    }

    return MalList.create(
        [MalSymbol.create("cons"), quasiquote(a[0]),
         quasiquote(MalList.create(MalObject.slice(a, 1)))]);
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
            else if( a0 == "quote" )
            {
                return ast[1];
            }
            else if( a0 == "quasiquote" )
            {
                quasiquote(ast[1]) @=> m;
                continue; // TCO
            }
            else if( a0 == "do" )
            {
                MalObject.slice(ast, 1, ast.size()) @=> MalObject forms[];
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
            subr.name => string name;

            if( name == "eval" )
            {
                return EVAL(args[0], subr.env);
            }
            else if( name == "swap!")
            {
                args[0]$MalAtom @=> MalAtom atom;
                atom.value() @=> MalObject value;
                args[1] @=> MalObject f;
                MalObject.slice(args, 2) @=> MalObject _args[];
                MalObject.append([f, value], _args) @=> _args;
                EVAL(MalList.create(_args), env) @=> MalObject _value;
                // NOTE: the DoSwap subr only puts a value into an atom
                return subr.call([atom, _value]);
            }
            else
            {
                return subr.call(args);
            }
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

repl_env.set("eval", MalSubr.create("eval", repl_env));

fun MalObject[] MalArgv(string args[])
{
    MalObject values[args.size()-1];

    for( 1 => int i; i < args.size(); i++ )
    {
        MalString.create(args[i]) @=> values[i-1];
    }

    return values;
}

// NOTE: normally I'd use \0, but strings are null-terminated...
String.split(Std.getenv("CHUCK_ARGS"), "\a") @=> string args[];
repl_env.set("*ARGV*", MalList.create(MalArgv(args)));

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

rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");

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
            Util.println(output);
        }
    }
}

if( args.size() > 1 )
{
    args[1] => string filename;
    rep("(load-file \"" + filename + "\")");
}
else
{
    main();
}
