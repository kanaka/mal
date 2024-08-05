import reader
import printer
import types
import types.MalException
import types.MalSymbol

// READ
READ = { str ->
    reader.read_str str
}

// EVAL
EVAL = { ast, env ->
    // println("EVAL: ${printer.pr_str(ast,true)}")

    switch (ast) {
        case MalSymbol:
            if (env.containsKey(ast.value)) return env.get(ast.value)
            throw new MalException("'${ast.value}' not found")
        case List:      if (types.vector_Q(ast)) {
                            return types.vector(ast.collect { EVAL(it, env) })
                        }
                        break;
        case Map:
            def new_hm = [:]
            ast.each { k,v ->
                new_hm[k] = EVAL(v, env)
            }
            return new_hm
        default:
            return ast
    }

    if (ast.size() == 0) return ast

    def el = ast.collect { EVAL(it, env) }
    def (f, args) = [el[0], el[1..-1]]
    f(args)
}

// PRINT
PRINT = { exp ->
    printer.pr_str exp, true
}

// REPL
repl_env = [
    "+": { a -> a[0]+a[1]},
    "-": { a -> a[0]-a[1]},
    "*": { a -> a[0]*a[1]},
    "/": { a -> a[0]/a[1]}]  // /
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
        println "Error: ${printer.pr_str(ex.obj, true)}"
    } catch(ex) {
        println "Error: $ex"
        ex.printStackTrace()
    }
}
