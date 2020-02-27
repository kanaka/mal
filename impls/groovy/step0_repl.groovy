// READ
READ = { str ->
    str
}

// EVAL
EVAL = { ast, env ->
    ast
}

// PRINT
PRINT = { exp ->
    exp
}

// REPL
REP = { str ->
    PRINT(EVAL(READ(str), [:]))
}

while (true) {
    line = System.console().readLine 'user> '
    if (line == null) {
        break
    }
    try {
        println REP(line)
    } catch(ex) {
        println "Error: $ex"
        ex.printStackTrace()
    }
}
