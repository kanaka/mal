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
        ast.size() > 0 &&
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

starts_with = { lst, sym ->
    lst.size() == 2 && lst[0].class == MalSymbol && lst[0].value == sym
}
qq_loop = { elt, acc ->
    if (types.list_Q(elt) && starts_with(elt, "splice-unquote")) {
        return [new MalSymbol("concat"), elt[1], acc]
    } else {
        return [new MalSymbol("cons"), quasiquote(elt), acc]
    }
}
qq_foldr = { xs ->
    def acc = []
    for (int i=xs.size()-1; 0<=i; i-=1) {
        acc = qq_loop(xs[i], acc)
    }
    return acc
}
quasiquote = { ast ->
    switch (ast) {
    case List:
        if (types.vector_Q(ast)) {
            return [new MalSymbol("vec"), qq_foldr(ast)]
        } else if (starts_with(ast, "unquote")) {
            return ast[1]
        } else {
            return qq_foldr(ast)
        }
    case MalSymbol: return [new MalSymbol("quote"), ast]
    case Map:       return [new MalSymbol("quote"), ast]
    default:        return ast
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
        env = let_env
        ast = ast[2]
        break // TCO
    case { it instanceof MalSymbol && it.value == "quote" }:
        return ast[1]
    case { it instanceof MalSymbol && it.value == "quasiquoteexpand" }:
        return quasiquote(ast[1])
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
REP("(def! *host-language* \"groovy\")")
REP("(def! not (fn* (a) (if a false true)))")
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");

if (this.args.size() > 0) {
    repl_env.set(new MalSymbol("*ARGV*"), this.args.drop(1) as List)
    REP("(load-file \"${this.args[0]}\")")
    System.exit(0)
}

REP("(println (str \"Mal [\" *host-language* \"]\"))")
while (true) {
    line = System.console().readLine 'user> '
    if (line == null) {
        break;
    }
    try {
        println REP(line)
    } catch(MalException ex) {
        println "Error: ${printer.pr_str(ex.obj, true)}"
    } catch(StackOverflowError ex) {
        println "Error: ${ex}"
    } catch(ex) {
        println "Error: $ex"
        ex.printStackTrace()
    }
}
