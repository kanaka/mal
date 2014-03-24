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
function is_pair(x) {
    return types.sequential_Q(x) && x.length > 0;
}

function quasiquote(ast) {
    if (!is_pair(ast)) {
        return [types.symbol("quote"), ast];
    } else if (ast[0].value === 'unquote') {
        return ast[1];
    } else if (is_pair(ast[0]) && ast[0][0].value === 'splice-unquote') {
        return [types.symbol("concat"), ast[0][1], quasiquote(ast.slice(1))];
    } else {
        return [types.symbol("cons"), quasiquote(ast[0]), quasiquote(ast.slice(1))];
    }
}

function is_macro_call(ast, env) {
    return types.list_Q(ast) &&
           types.symbol_Q(ast[0]) &&
           env.find(ast[0].value) &&
           env.get(ast[0].value)._ismacro_;
}

function macroexpand(ast, env) {
    while (is_macro_call(ast, env)) {
        var mac = env.get(ast[0]);
        ast = mac.apply(mac, ast.slice(1));
    }
    return ast;
}

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
        //console.log("EVAL:", types._pr_str(ast, true));
        if (!types.list_Q(ast)) {
            return eval_ast(ast, env);
        }
    
        // apply list
        ast = macroexpand(ast, env);
        if (!types.list_Q(ast)) { return ast; }

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
        case "quote":
            return a1;
        case "quasiquote":
            return EVAL(quasiquote(a1), env);
        case 'defmacro!':
            var func = EVAL(a2, env);
            func._ismacro_ = true;
            return env.set(a1, func);
        case 'macroexpand':
            return macroexpand(a1, env);
        case "js*":
            return eval(a1.toString());
        case ".":
            var el = eval_ast(ast.slice(2), env),
                f = eval(a1.toString());
            return f.apply(f, el);
        case "try*":
            try {
                return EVAL(a1, env);
            } catch (exc) {
                if (a2 && a2[0].value === "catch*") {
                    if (exc instanceof Error) { exc = exc.message; }
                    return EVAL(a2[2], new types.Env(env, [a2[1]], [exc]));
                } else {
                    throw exc;
                }
            }
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

_ref('readline', readline.readline)
_ref('read-string', reader.read_str);
_ref('eval', function(ast) { return EVAL(ast, repl_env); });
_ref('slurp', function(f) {
    return require('fs').readFileSync(f, 'utf-8');
});
_ref('slurp-do', function(f) {
    return '(do ' + require('fs').readFileSync(f, 'utf-8') + ')';
});

// Defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))");
rep("(def! load-file (fn* (f) (eval (read-string (slurp-do f)))))");

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
