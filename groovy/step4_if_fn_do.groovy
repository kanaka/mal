import reader
import printer
import types
import types.MalException
import types.MalSymbol
import types.MalFunc
import env.Env
import core

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
    case { it instanceof MalSymbol && it.value == "do" }:
        return eval_ast(ast[1..-1], env)[-1]
    case { it instanceof MalSymbol && it.value == "if" }:
        def cond = EVAL(ast[1], env)
        if (cond == false || cond == null) {
            if (ast.size > 3) {
                return EVAL(ast[3], env)
            } else {
                return null
            }
        } else {
            return EVAL(ast[2], env)
        }
    case { it instanceof MalSymbol && it.value == "fn*" }:
        return new MalFunc(EVAL, ast[2], env, ast[1])
    default:
        def el = eval_ast(ast, env)
        def (f, args) = [el[0], el.size() > 1 ? el[1..-1] : []]
        f(args)
    }
}

// PRINT
PRINT = { exp ->
    printer.pr_str exp, true
}

// REPL
repl_env = new Env();
REP = { str ->
    PRINT(EVAL(READ(str), repl_env))
}

// core.EXT: defined using Groovy
core.ns.each { k,v ->
    repl_env.set(new MalSymbol(k), v)
}

// core.mal: defined using mal itself
REP("(def! not (fn* (a) (if a false true)))")


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
