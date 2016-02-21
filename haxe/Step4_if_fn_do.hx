import Compat;
import types.Types.MalType;
import types.Types.*;
import reader.*;
import printer.*;
import env.*;
import core.*;

class Step4_if_fn_do {
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
            return EVAL(alst[2], let_env);
        case MalSymbol("do"):
            return last(eval_ast(MalList(alst.slice(1)), env));
        case MalSymbol("if"):
            var cond = EVAL(alst[1], env);
            if (cond != MalFalse && cond != MalNil) {
                return EVAL(alst[2], env);
            } else if (alst.length > 3) {
                return EVAL(alst[3], env);
            } else {
                return MalNil;
            }
        case MalSymbol("fn*"):
            return MalFunc(function (args) {
                return EVAL(alst[2], new Env(env, _list(alst[1]), args));
            },null,null,null,false,nil);
        case _:
            var el = eval_ast(ast, env);
            var lst = _list(el);
            switch (first(el)) {
                case MalFunc(f,_,_,_,_,_): return f(_list(el).slice(1));
                case _: throw "Call of non-function";
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

        // core.mal: defined using the language itself
        rep("(def! not (fn* (a) (if a false true)))");

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
