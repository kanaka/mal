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

fun int startsWith(MalObject a[], string sym)
{
    if (a.size() != 2)
    {
        return false;
    }

    a[0] @=> MalObject a0;
    return a0.type == "symbol" && a0.stringValue == sym;
}

fun MalList qqLoop(MalObject elt, MalList acc)
{
    if( elt.type == "list" )
    {
        elt.malObjectValues() @=> MalObject ast[];

        if( startsWith(ast, "splice-unquote") )
        {
            return MalList.create([MalSymbol.create("concat"), ast[1], acc]);
        }
    }
    return MalList.create([MalSymbol.create("cons"), quasiquote(elt), acc]);
}

fun MalList qqFoldr(MalObject a[])
{
    MalObject empty[0];  //  empty, but typed
    MalList.create(empty) @=> MalList acc;

    for( a.size() - 1 => int i; 0 <= i; i-- )
    {
        qqLoop(a[i], acc) @=> acc;
    }

    return acc;
}

fun MalObject quasiquote(MalObject ast)
{
    ast.type => string type;
    if (type == "list") {
        ast.malObjectValues() @=> MalObject a[];
        if (startsWith(a, "unquote"))
        {
            return a[1];
        }
        return qqFoldr(a);
    }

    if (type == "vector")
    {
        return MalList.create([MalSymbol.create("vec"), qqFoldr(ast.malObjectValues())]);
    }

    if (type == "symbol" || type == "hashmap")
    {
        return MalList.create([MalSymbol.create("quote"), ast]);
    }

    return ast;
}

fun MalObject EVAL(MalObject m, Env env)
{
  while( true )
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
            else if( a0 == "defmacro!" )
            {
                ast[1].stringValue => string a1;

                EVAL(ast[2], env) @=> MalObject value;
                if( value.type == "error" )
                {
                    return value;
                }

                value.clone() @=> value;
                true => (value$Func).isMacro;

                env.set(a1, value);
                return value;
            }
            else if( a0 == "do" )
            {
              for( 1 => int i; i < ast.size() - 1; i++ )
              {
                EVAL(ast[i], env) @=> MalObject value;
                if( value.type == "error" )
                {
                    return value;
                }
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
        else if( first.type == "subr" )
        {
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
            first$MalSubr @=> MalSubr subr;
            return subr.call(args);
        }
        else if( first.type == "func" )
        {
            first$Func @=> Func func;
            if( func.isMacro )
            {
                MalObject.slice(ast, 1) @=> MalObject args[];
                Env.create(func.env, func.args, args) @=> Env eval_env;
                EVAL(func.ast, eval_env) @=> m;
                continue; // TCO
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
            Env.create(func.env, func.args, args) @=> Env eval_env;
            eval_env @=> env;
            func.ast @=> m;
            continue; // TCO
        }
    }
    else
    {
        return m;
    }
  }
  Util.panic("Programmer error: TCO loop left incorrectly");
  return null;
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
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");

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

if( args.size() > 0 )
{
    args[0] => string filename;
    rep("(load-file \"" + filename + "\")");
}
else
{
    main();
}
