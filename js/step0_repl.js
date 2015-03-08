if (typeof module !== 'undefined') {
    var readline = require('./node_readline');
    var printer = require('./printer');
}

// read
function READ(str) {
    return str;
}

// eval
function EVAL(ast, env) {
    return ast;
}

// print
function PRINT(exp) {
    return exp;
}

// repl
var rep = function(str) { return PRINT(EVAL(READ(str), {})); };

// repl loop
if (typeof require !== 'undefined' && require.main === module) {
    // Synchronous node.js commandline mode
    while (true) {
        var line = readline.readline("user> ");
        if (line === null) { break; }
        try {
            if (line) { printer.println(rep(line)); }
        } catch (exc) {

            if (exc.stack) { printer.println(exc.stack); }
            else           { printer.println(exc); }
        }
    }
}
