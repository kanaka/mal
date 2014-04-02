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
        return env.get(ast);
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
    while (true) {
        if (!types.list_Q(ast)) {
            return eval_ast(ast, env);
        }
    
        // apply list
        var a0 = ast[0], a1 = ast[1], a2 = ast[2], a3 = ast[3];
        switch (a0.value) {
        case "def!":
            var res = EVAL(a2, env);
            return env.set(a1, res);
        case "let*":
            var let_env = new types.Env(env);
            for (var i=0; i < a1.length; i+=2) {
                let_env.set(a1[i].value, EVAL(a1[i+1], let_env));
            }
            return EVAL(a2, let_env);
        case "do":
            eval_ast(ast.slice(1, -1), env);
            ast = ast[ast.length-1];
            break;
        case "if":
            var cond = EVAL(a1, env);
            if (cond === null || cond === false) {
                ast = (typeof a3 !== "undefined") ? a3 : null;
            } else {
                ast = a2;
            }
            break;
        case "fn*":
            return types.new_function(EVAL, a2, env, a1);
        default:
            var el = eval_ast(ast, env), f = el[0], meta = f.__meta__;
            if (meta && meta.exp) {
                ast = meta.exp;
                env = new types.Env(meta.env, meta.params, el.slice(1));
            } else {
                return f.apply(f, el.slice(1));
            }
        }
    }
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
var repl_env = new types.Env();
var rep = function(str) { return PRINT(EVAL(READ(str), repl_env)); };
_ref = function (k,v) { repl_env.set(k, v); }

// Import types functions
for (var n in types.ns) { repl_env.set(n, types.ns[n]); }

_ref('read-string', reader.read_str);
_ref('eval', function(ast) { return EVAL(ast, repl_env); });
_ref('slurp', function(f) {
    return require('fs').readFileSync(f, 'utf-8');
});

// Defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");

if (typeof process !== 'undefined' && process.argv.length > 2) {
    for (var i=2; i < process.argv.length; i++) {
        rep('(load-file "' + process.argv[i] + '")');
    }
} else if (typeof require === 'undefined') {
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
