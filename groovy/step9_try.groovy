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
macro_Q = { ast, env ->
    if (types.list_Q(ast) &&
        ast[0].class == MalSymbol &&
        env.find(ast[0])) {
        def obj = env.get(ast[0])
        if (obj instanceof MalFunc && obj.ismacro) {
            return true
        }
    }
    return false
}
macroexpand = { ast, env ->
    while (macro_Q(ast, env)) {
        def mac = env.get(ast[0])
        ast = mac(ast.drop(1))
    }
    return ast
}

pair_Q = { ast -> types.sequential_Q(ast) && ast.size() > 0}
quasiquote = { ast ->
    if (! pair_Q(ast)) {
        [new MalSymbol("quote"), ast]
    } else if (ast[0].class == MalSymbol &&
               ast[0].value == "unquote") {
        ast[1]
    } else if (pair_Q(ast[0]) && ast[0][0].class == MalSymbol && 
               ast[0][0].value == "splice-unquote") {
        [new MalSymbol("concat"), ast[0][1], quasiquote(ast.drop(1))]
    } else {
        [new MalSymbol("cons"), quasiquote(ast[0]), quasiquote(ast.drop(1))]
    }
}

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
  while (true) {
    //println("EVAL: ${printer.pr_str(ast,true)}")
    if (! types.list_Q(ast)) return eval_ast(ast, env)

    ast = macroexpand(ast, env)
    if (! types.list_Q(ast)) return ast

    switch (ast[0]) {
    case { it instanceof MalSymbol && it.value == "def!" }:
        return env.set(ast[1], EVAL(ast[2], env))
    case { it instanceof MalSymbol && it.value == "let*" }:
        def let_env = new Env(env)
        for (int i=0; i < ast[1].size(); i += 2) {
            let_env.set(ast[1][i], EVAL(ast[1][i+1], let_env))
        }
        env = let_env
        ast = ast[2]
        break // TCO
    case { it instanceof MalSymbol && it.value == "quote" }:
        return ast[1]
    case { it instanceof MalSymbol && it.value == "quasiquote" }:
        ast = quasiquote(ast[1])
        break // TCO
    case { it instanceof MalSymbol && it.value == "defmacro!" }:
        def f = EVAL(ast[2], env)
        f.ismacro = true
        return env.set(ast[1], f)
    case { it instanceof MalSymbol && it.value == "macroexpand" }:
        return macroexpand(ast[1], env)
    case { it instanceof MalSymbol && it.value == "try*" }:
        try {
            return EVAL(ast[1], env)
        } catch(exc) {
            if (ast.size() > 2 && 
                    ast[2][0] instanceof MalSymbol &&
                    ast[2][0].value == "catch*") {
                def e = null
                if (exc instanceof MalException) {
                    e = exc.obj
                } else {
                    e = exc.message
                }
                return EVAL(ast[2][2], new Env(env, [ast[2][1]], [e]))
            } else {
                throw exc
            }
        }
    case { it instanceof MalSymbol && it.value == "do" }:
        ast.size() > 2 ? eval_ast(ast[1..-2], env) : null
        ast = ast[-1]
        break // TCO
    case { it instanceof MalSymbol && it.value == "if" }:
        def cond = EVAL(ast[1], env)
        if (cond == false || cond == null) {
            if (ast.size > 3) {
                ast = ast[3]
                break // TCO
            } else {
                return null
            }
        } else {
            ast = ast[2]
            break // TCO
        }
    case { it instanceof MalSymbol && it.value == "fn*" }:
        return new MalFunc(EVAL, ast[2], env, ast[1])
    default:
        def el = eval_ast(ast, env)
        def (f, args) = [el[0], el.drop(1)]
        if (f instanceof MalFunc) {
            env = new Env(f.env, f.params, args)
            ast = f.ast
            break // TCO
        } else {
            return f(args)
        }
    }
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
repl_env.set(new MalSymbol("eval"), { a -> EVAL(a[0], repl_env)})
repl_env.set(new MalSymbol("*ARGV*"), this.args as List)

// core.mal: defined using mal itself
REP("(def! not (fn* (a) (if a false true)))")
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
REP("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))");


if (this.args.size() > 0) {
    repl_env.set(new MalSymbol("*ARGV*"), this.args.drop(1) as List)
    REP("(load-file \"${this.args[0]}\")")
    System.exit(0)
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
    } catch(StackOverflowError ex) {
        println "Error: ${ex}"
    } catch(ex) {
        println "Error: $ex"
        ex.printStackTrace()
    }
}
