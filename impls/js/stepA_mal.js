if (typeof module !== 'undefined') {
    var types = require('./types');
    var readline = require('./node_readline');
    var reader = require('./reader');
    var printer = require('./printer');
    var Env = require('./env').Env;
    var core = require('./core');
}

// read
function READ(str) {
    return reader.read_str(str);
}

// eval
function qqLoop (acc, elt) {
    if (types._list_Q(elt) && elt.length
        && types._symbol_Q(elt[0]) && elt[0].value == 'splice-unquote') {
        return [types._symbol("concat"), elt[1], acc];
    } else {
        return [types._symbol("cons"), quasiquote (elt), acc];
    }
}
function quasiquote(ast) {
    if (types._list_Q(ast) && 0<ast.length
        && types._symbol_Q(ast[0]) && ast[0].value == 'unquote') {
        return ast[1];
    } else if (types._list_Q(ast)) {
        return ast.reduceRight(qqLoop,[]);
    } else if (types._vector_Q(ast)) {
        return [types._symbol("vec"), ast.reduceRight(qqLoop,[])];
    } else if (types._symbol_Q(ast) || types._hash_map_Q(ast)) {
        return [types._symbol("quote"), ast];
    } else {
        return ast;
    }
}

function _EVAL(ast, env) {
    while (true) {
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
    // apply list
    if (ast.length === 0) {
        return ast;
    }

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
        ast = a2;
        env = let_env;
        break;
    case "quote":
        return a1;
    case "quasiquote":
        ast = quasiquote(a1);
        break;
    case 'defmacro!':
        var func = types._clone(EVAL(a2, env));
        func._ismacro_ = true;
        if (!a1.constructor || a1.constructor.name !== 'Symbol') {
            throw new Error("env.get key must be a symbol")
        }
        return env.set(a1.value, func);
    case "try*":
        try {
            return EVAL(a1, env);
        } catch (exc) {
            if (a2 && a2[0].value === "catch*") {
                if (exc instanceof Error) { exc = exc.message; }
                return EVAL(a2[2], new Env(env, [a2[1]], [exc]));
            } else {
                throw exc;
            }
        }
    case "do":
        for (var i=1; i < ast.length - 1; i++) {
            EVAL(ast[i], env);
        }
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
        return types._function(EVAL, Env, a2, env, a1);
    default:
        var f = EVAL(a0, env);
        if (f._ismacro_) {
            ast = f.apply(f, ast.slice(1));
            break;
        }
        var args = ast.slice(1).map(function(a) { return EVAL(a, env); });
        if (f.__ast__) {
            ast = f.__ast__;
            env = f.__gen_env__(args);
        } else {
            return f.apply(f, args);
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
    return printer._pr_str(exp, true);
}

// repl
var repl_env = new Env();
var rep = function(str) { return PRINT(EVAL(READ(str), repl_env)); };

// core.js: defined using javascript
for (var n in core.ns) { repl_env.set(n, core.ns[n]); }
repl_env.set('eval', function(ast) {
    return EVAL(ast, repl_env); });
repl_env.set('*ARGV*', []);

// core.mal: defined using the language itself
rep("(def! *host-language* \"javascript\")")
rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");

if (typeof process !== 'undefined' && process.argv.length > 2) {
    repl_env.set('*ARGV*', process.argv.slice(3));
    rep('(load-file "' + process.argv[2] + '")');
    process.exit(0);
}

// repl loop
if (typeof require !== 'undefined' && require.main === module) {
    // Synchronous node.js commandline mode
    rep("(println (str \"Mal [\" *host-language* \"]\"))");
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
