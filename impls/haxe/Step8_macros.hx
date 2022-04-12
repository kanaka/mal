import Compat;
import types.Types.MalType;
import types.Types.*;
import types.MalException;
import reader.*;
import printer.*;
import env.*;
import core.*;

class Step8_macros {
    // READ
    static function READ(str:String):MalType {
        return Reader.read_str(str);
    }

    // EVAL
    static function qq_loop(elt:MalType, acc:MalType) {
        switch elt {
            case MalList([MalSymbol("splice-unquote"), arg]):
                return MalList([MalSymbol("concat"), arg, acc]);
            case _:
                return MalList([MalSymbol("cons"), quasiquote(elt), acc]);
        }
    }
    static function qq_foldr(xs:Array<MalType>) {
        var acc = MalList([]);
        for (i in 1 ... xs.length+1) {
            acc = qq_loop (xs[xs.length-i], acc);
        }
        return acc;
    }
    static function quasiquote(ast:MalType) {
        return switch(ast) {
            case MalList([MalSymbol("unquote"), arg]): arg;
            case MalList(l): qq_foldr(l);
            case MalVector(l): MalList([MalSymbol("vec"), qq_foldr(l)]);
            case MalSymbol(_) | MalHashMap(_): MalList([MalSymbol("quote"), ast]);
            case _: ast;
        }
    }

    static function EVAL(ast:MalType, env:Env):MalType {
      while (true) {
        var dbgeval = env.get("DEBUG-EVAL");
        if (dbgeval != null && dbgeval != MalFalse && dbgeval != MalNil)
            Compat.println("EVAL: " + PRINT(ast));
        var alst;
        switch (ast) {
            case MalSymbol(s):
                 var  res = env.get(s);
                 if (res == null) throw "'" + s + "' not found";
                 return res;
            case MalList(l):
                 alst = l;
            case MalVector(l):
                return MalVector(l.map(function(x) { return EVAL(x, env); }));
            case MalHashMap(m):
                var new_map = new Map<String,MalType>();
                for (k in m.keys()) {
                    new_map[k] = EVAL(m[k], env);
                }
                return MalHashMap(new_map);
            case _: return ast;
        }
        // apply
        if (alst.length == 0) { return ast; }
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
        case MalSymbol("do"):
            for (i in 1...alst.length-1)
                EVAL(alst[i], env);
            ast = alst[alst.length-1];
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
            switch ( EVAL(alst[0], env)) {
                case MalFunc(f,a,e,params,ismacro,_):
                    if (ismacro) {
                        ast = f(alst.slice(1));
                        continue; // TCO
                    }
                    var args = alst.slice(1).map(function(x) { return EVAL(x, env); });
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
        rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");
        rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");


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
                if (Type.getClass(exc) == MalException) {
                    Compat.println("Error: " + Printer.pr_str(exc.obj, true));
                } else {
                    Compat.println("Error: " + exc);
                };
            }
        }
    }
}
