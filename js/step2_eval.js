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
function eval_ast(ast, env) {
    if (types.symbol_Q(ast)) {
        return env[ast];
    } else if (types.list_Q(ast)) {
        return ast.map(function(a) { return EVAL(a, env); });
    } else if (types.vector_Q(ast)) {
        var v = ast.map(function(a) { return EVAL(a, env); });
        v.__isvector__ = true;
        return v;
    } else if (types.hash_map_Q(ast)) {
        var new_hm = {};
        for (k in ast) {
            new_hm[EVAL(k, env)] = EVAL(ast[k], env);
        }
        return new_hm;
    } else {
        return ast;
    }
}

function _EVAL(ast, env) {
    if (!types.list_Q(ast)) {
        return eval_ast(ast, env);
    }

    // apply list
    var el = eval_ast(ast, env), f = el[0];
    return f.apply(f, el.slice(1));
}

function EVAL(ast, env) {
    var result = _EVAL(ast, env);
    return (typeof result !== "undefined") ? result : null;
}

// print
function PRINT(exp) {
    return types._pr_str(exp, true);
}

// repl
repl_env = {};
var rep = function(str) { return PRINT(EVAL(READ(str), repl_env)); };

repl_env['+'] = function(a,b){return a+b;};
repl_env['-'] = function(a,b){return a-b;};
repl_env['*'] = function(a,b){return a*b;};
repl_env['/'] = function(a,b){return a/b;};

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
