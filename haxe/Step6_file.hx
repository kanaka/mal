import Compat;
import types.Types.MalType;
import types.Types.*;
import reader.*;
import printer.*;
import env.*;
import core.*;

class Step6_file {
    // READ
    static function READ(str:String):MalType {
        return Reader.read_str(str);
    }

    // EVAL
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
