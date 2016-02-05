import Compat;
import types.Types.MalType;
import types.Types.*;
import types.MalException;
import reader.*;
import printer.*;
import env.*;
import core.*;
import haxe.rtti.Meta;

class Step9_try {
    // READ
    static function READ(str:String):MalType {
        return Reader.read_str(str);
    }

    // EVAL
    static function is_pair(ast:MalType) {
        return switch (ast) {
            case MalList(l) | MalVector(l): l.length > 0;
            case _: false;
        }
    }

    static function quasiquote(ast:MalType) {
        if (!is_pair(ast)) {
            return MalList([MalSymbol("quote"), ast]);
        } else {
            var a0 = first(ast);
            if (_equal_Q(a0, MalSymbol("unquote"))) {
                    return _nth(ast, 1);
            } else if (is_pair(a0)) {
                var a00 = first(a0);
                if (_equal_Q(a00, MalSymbol("splice-unquote"))) {
                    return MalList([MalSymbol("concat"),
                                    _nth(a0, 1),
                                    quasiquote(rest(ast))]);
                }
            }
            return MalList([MalSymbol("cons"),
                            quasiquote(a0),
                            quasiquote(rest(ast))]);
        }
    }

    static function is_macro(ast:MalType, env:Env) {
        return switch(ast) {
            case MalList(a):
                var a0 = a[0];
                return symbol_Q(a0) &&
                       env.find(a0) != null &&
                       _macro_Q(env.get(a0));
            case _: false;
        }
    }

    static function macroexpand(ast:MalType, env:Env) {
        while (is_macro(ast, env)) {
            var mac = env.get(first(ast));
            switch (mac) {
                case MalFunc(f,_,_,_,_,_):
                    ast = f(_list(ast).slice(1));
                case _: break;
            }
        }
        return ast;
    }

    static function eval_ast(ast:MalType, env:Env) {
        return switch (ast) {
            case MalSymbol(s): env.get(ast);
            case MalList(l):
                MalList(l.map(function(x) { return EVAL(x, env); }));
            case MalVector(l):
                MalVector(l.map(function(x) { return EVAL(x, env); }));
            case MalHashMap(m):
                var new_map = new Map<String,MalType>();
                for (k in m.keys()) {
                    new_map[k] = EVAL(m[k], env);
                }
                MalHashMap(new_map);
            case _: ast;
        }
    }

    static function EVAL(ast:MalType, env:Env):MalType {
      while (true) {
        if (!list_Q(ast)) { return eval_ast(ast, env); }

        // apply
        ast = macroexpand(ast, env);
        if (!list_Q(ast)) { return eval_ast(ast, env); }

        var alst = _list(ast);
        switch (alst[0]) {
        case MalSymbol("def!"):
            return env.set(alst[1], EVAL(alst[2], env));
        case MalSymbol("let*"):
            var let_env = new Env(env);
            switch (alst[1]) {
                case MalList(l) | MalVector(l):
                    for (i in 0...l.length) {
                        if ((i%2) > 0) { continue; }
                        let_env.set(l[i], EVAL(l[i+1], let_env));
                    }
                case _: throw "Invalid let*";
            }
            ast = alst[2];
            env = let_env;
            continue; // TCO
        case MalSymbol("quote"):
            return alst[1];
        case MalSymbol("quasiquote"):
            ast = quasiquote(alst[1]);
            continue; // TCO
        case MalSymbol("defmacro!"):
            var func = EVAL(alst[2], env);
            return switch (func) {
                case MalFunc(f,ast,e,params,_,_):
                    env.set(alst[1], MalFunc(f,ast,e,params,true,nil));
                case _:
                    throw "Invalid defmacro! call";
            }
        case MalSymbol("macroexpand"):
            return macroexpand(alst[1], env);
        case MalSymbol("try*"):
            try {
                return EVAL(alst[1], env);
            } catch (err:Dynamic) {
                if (alst.length > 2) {
                    switch (alst[2]) {
                        case MalList([MalSymbol("catch*"), a21, a22]): 
                            var exc;
                            if (Type.getClass(err) == MalException) {
                                exc = err.obj;
                            } else {
                                exc = MalString(Std.string(err));
                            };
                            return EVAL(a22, new Env(env, [a21], [exc]));
                        case _:
                            throw err;
                    }
                } else {
                    throw err;
                }
            }
        case MalSymbol("do"):
            var el = eval_ast(MalList(alst.slice(1, alst.length-1)), env);
            ast = last(ast);
            continue; // TCO
        case MalSymbol("if"):
            var cond = EVAL(alst[1], env);
            if (cond != MalFalse && cond != MalNil) {
                ast = alst[2];
            } else if (alst.length > 3) {
                ast = alst[3];
            } else {
                return MalNil;
            }
            continue; // TCO
        case MalSymbol("fn*"):
            return MalFunc(function (args) {
                return EVAL(alst[2], new Env(env, _list(alst[1]), args));
            },alst[2],env,alst[1],false,nil);
        case _:
            var el = eval_ast(ast, env);
            var lst = _list(el);
            switch (first(el)) {
                case MalFunc(f,a,e,params,_,_):
                    var args = _list(el).slice(1);
                    if (a != null) {
                        ast = a;
                        env = new Env(e, _list(params), args);
                        continue; // TCO
                    } else {
                        return f(args);
                    }
                case _: throw "Call of non-function";
            }
        }
      }
    }

    // PRINT
    static function PRINT(exp:MalType):String {
        return Printer.pr_str(exp, true);
    }

    // repl
    static var repl_env = new Env(null);

    static function rep(line:String):String {
        return PRINT(EVAL(READ(line), repl_env));
    }

    public static function main() {
        // core.EXT: defined using Haxe
        for (k in Core.ns.keys()) {
            repl_env.set(MalSymbol(k), MalFunc(Core.ns[k],null,null,null,false,nil));
        }

        var evalfn = MalFunc(function(args) {
            return EVAL(args[0], repl_env);
        },null,null,null,false,nil);
        repl_env.set(MalSymbol("eval"), evalfn);

        var cmdargs = Compat.cmdline_args();
        var argarray = cmdargs.map(function(a) { return MalString(a); });
        repl_env.set(MalSymbol("*ARGV*"), MalList(argarray.slice(1)));

        // core.mal: defined using the language itself
        rep("(def! not (fn* (a) (if a false true)))");
        rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");
        rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
        rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))");


        if (cmdargs.length > 0) {
            rep('(load-file "${cmdargs[0]}")');
            Compat.exit(0);
        }

        while (true) {
            try {
                var line = Compat.readline("user> ");
                if (line == "") { continue; }
                Compat.println(rep(line));
            } catch (exc:BlankLine) {
                continue;
            } catch (exc:haxe.io.Eof) {
                Compat.exit(0);
            } catch (exc:Dynamic) {
                Compat.println(exc);
            }
        }
    }
}
