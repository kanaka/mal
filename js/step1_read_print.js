var types = require('./types');
var reader = require('./reader');
if (typeof module !== 'undefined') {
    var readline = require('./node_readline');
}

// read
function READ(str) {
    return reader.read_str(str);
}

// eval
function EVAL(ast, env) {
    return ast;
}

// print
function PRINT(exp) {
    return types._pr_str(exp, true);
}

// repl
var re = function(str) { return EVAL(READ(str), {}); };
var rep = function(str) { return PRINT(EVAL(READ(str), {})); };

if (typeof require === 'undefined') {
    // Asynchronous browser mode
    readline.rlwrap(function(line) { return rep(line); },
                    function(exc) {
                        if (exc instanceof reader.BlankException) { return; }
                        if (exc.stack) { console.log(exc.stack); } else { console.log(exc); }
                    });
} else if (require.main === module) {
    // Synchronous node.js commandline mode
    while (true) {
        var line = readline.readline("user> ");
        if (line === null) { break; }
        try {
            if (line) { console.log(rep(line)); }
        } catch (exc) {
            if (exc instanceof reader.BlankException) { continue; }
            if (exc.stack) { console.log(exc.stack); } else { console.log(exc); }
        }
    }
} else {
    exports.rep = rep;
}
