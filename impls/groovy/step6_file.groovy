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
EVAL = { ast, env ->
  while (true) {
    def dbgevalenv = env.find("DEBUG-EVAL");
    if (dbgevalenv != null) {
      def dbgeval = env.get("DEBUG-EVAL");
      if (dbgeval != null && dbgeval != false) {
        println("EVAL: ${printer.pr_str(ast,true)}")
      }
   }

    switch (ast) {
        case MalSymbol: return env.get(ast.value);
        case List:      if (types.vector_Q(ast)) {
                            return types.vector(ast.collect { EVAL(it, env) })
                        }
                        break;
        case Map:       def new_hm = [:]
                        ast.each { k,v ->
                            new_hm[k] = EVAL(v, env)
                        }
                        return new_hm
        default:        return ast
    }

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
    case { it instanceof MalSymbol && it.value == "do" }:
        ast.size() > 2 ? ast[1..-2].collect { EVAL(it, env) } : null
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
        def el = ast.collect { EVAL(it, env) }
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
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")

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
        println "Error: ${printer.pr_str(ex.obj, true)}"
    } catch(StackOverflowError ex) {
        println "Error: ${ex}"
    } catch(ex) {
        println "Error: $ex"
        ex.printStackTrace()
    }
}
