if (typeof module !== 'undefined') {
    var types = require('./types');
    var readline = require('./node_readline');
    var reader = require('./reader');
    var printer = require('./printer');
    var Env = require('./env').Env;
}

// read
function READ(str) {
    return reader.read_str(str);
}

// eval
function _EVAL(ast, env) {
    // Show a trace if DEBUG-EVAL is enabled.
    var dbgevalenv = env.find("DEBUG-EVAL");
    if (dbgevalenv !== null) {
        var dbgeval = env.get("DEBUG-EVAL");
        if (dbgeval !== null && dbgeval !== false)
            printer.println("EVAL:", printer._pr_str(ast, true));
    }
    // Non-list types.
    if (types._symbol_Q(ast)) {
        return env.get(ast.value);
    } else if (types._list_Q(ast)) {
        // Exit this switch.
    } else if (types._vector_Q(ast)) {
        var v = ast.map(function(a) { return EVAL(a, env); });
        v.__isvector__ = true;
        return v;
    } else if (types._hash_map_Q(ast)) {
        var new_hm = {};
        for (k in ast) {
            new_hm[k] = EVAL(ast[k], env);
        }
        return new_hm;
    } else {
        return ast;
    }

    if (ast.length === 0) {
        return ast;
    }

    // apply list
    var a0 = ast[0], a1 = ast[1], a2 = ast[2], a3 = ast[3];
    switch (a0.value) {
    case "def!":
        var res = EVAL(a2, env);
        if (!a1.constructor || a1.constructor.name !== 'Symbol') {
            throw new Error("env.get key must be a symbol")
        }
        return env.set(a1.value, res);
    case "let*":
        var let_env = new Env(env);
        for (var i=0; i < a1.length; i+=2) {
            if (!a1[i].constructor || a1[i].constructor.name !== 'Symbol') {
                throw new Error("env.get key must be a symbol")
            }
            let_env.set(a1[i].value, EVAL(a1[i+1], let_env));
        }
        return EVAL(a2, let_env);
    default:
        var f = EVAL(a0, env);
        var args = ast.slice(1).map(function(a) { return EVAL(a, env); });
        return f.apply(f, args);
    }
}

function EVAL(ast, env) {
    var result = _EVAL(ast, env);
    return (typeof result !== "undefined") ? result : null;
}

// print
function PRINT(exp) {
    return printer._pr_str(exp, true);
}

// repl
var repl_env = new Env();
var rep = function(str) { return PRINT(EVAL(READ(str), repl_env)); };

repl_env.set('+', function(a,b){return a+b;});
repl_env.set('-', function(a,b){return a-b;});
repl_env.set('*', function(a,b){return a*b;});
repl_env.set('/', function(a,b){return a/b;});

// repl loop
if (typeof require !== 'undefined' && require.main === module) {
    // Synchronous node.js commandline mode
    while (true) {
        var line = readline.readline("user> ");
        if (line === null) { break; }
        try {
            if (line) { printer.println(rep(line)); }
        } catch (exc) {
            if (exc instanceof reader.BlankException) { continue }
            if (exc instanceof Error) { console.warn(exc.stack) }
            else { console.warn("Error: " + printer._pr_str(exc, true)) }
        }
    }
}
