import reader
import printer
import types.MalException

// READ
READ = { str ->
    reader.read_str str
}

// EVAL
EVAL = { ast, env ->
    ast
}

// PRINT
PRINT = { exp ->
    printer.pr_str exp, true
}

// REPL
REP = { str ->
    PRINT(EVAL(READ(str), [:]))
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
