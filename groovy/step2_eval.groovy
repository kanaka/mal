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
eval_ast = { ast, env ->
    switch (ast) {
        case MalSymbol:
            if (env.containsKey(ast.value)) return env.get(ast.value)
            throw new MalException("'${ast.value}' not found")
        case List:
            return types.vector_Q(ast) ?
                types.vector(ast.collect { EVAL(it,env) }) :
                ast.collect { EVAL(it,env) }
        case Map:
            def new_hm = [:]
            ast.each { k,v ->
                new_hm[EVAL(k, env)] = EVAL(v, env)
            }
            return new_hm
        default:
            return ast
    }
}

EVAL = { ast, env ->
    if (! types.list_Q(ast)) return eval_ast(ast, env)

    def el = eval_ast(ast, env)
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
        println "Error: ${ex.message}"
    } catch(ex) {
        println "Error: $ex"
        ex.printStackTrace()
    }
}
