import reader
import printer
import types
import types.MalException
import types.MalSymbol
import env.Env

// READ
READ = { str ->
    reader.read_str str
}

// EVAL
eval_ast = { ast, env ->
    switch (ast) {
        case MalSymbol: return env.get(ast);
        case List:      return types.vector_Q(ast) ?
                            types.vector(ast.collect { EVAL(it,env) }) :
                            ast.collect { EVAL(it,env) }
        case Map:       def new_hm = [:]
                        ast.each { k,v ->
                            new_hm[EVAL(k, env)] = EVAL(v, env)
                        }
                        return new_hm
        default:        return ast
    }
}

EVAL = { ast, env ->
    //println("EVAL: ${printer.pr_str(ast,true)}")
    if (! types.list_Q(ast)) return eval_ast(ast, env)
    if (ast.size() == 0) return ast

    switch (ast[0]) {
    case { it instanceof MalSymbol && it.value == "def!" }:
        return env.set(ast[1], EVAL(ast[2], env))
    case { it instanceof MalSymbol && it.value == "let*" }:
        def let_env = new Env(env)
        for (int i=0; i < ast[1].size(); i += 2) {
            let_env.set(ast[1][i], EVAL(ast[1][i+1], let_env))
        }
        return EVAL(ast[2], let_env)
    default:
        def el = eval_ast(ast, env)
        def (f, args) = [el[0], el[1..-1]]
        f(args)
    }
}

// PRINT
PRINT = { exp ->
    printer.pr_str exp, true
}

// REPL
repl_env = new Env();
repl_env.set(new MalSymbol("+"), { a -> a[0]+a[1]});
repl_env.set(new MalSymbol("-"), { a -> a[0]-a[1]});
repl_env.set(new MalSymbol("*"), { a -> a[0]*a[1]});
repl_env.set(new MalSymbol("/"), { a -> a[0]/a[1]});  // /
REP = { str ->
    PRINT(EVAL(READ(str), repl_env))
}

while (true) {
    line = System.console().readLine 'user> '
    if (line == null) {
        break;
    }
    try {
        println REP(line)
    } catch(MalException ex) {
        println "Error: ${ex.message}"
    } catch(ex) {
        println "Error: $ex"
        ex.printStackTrace()
    }
}
