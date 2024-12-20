import Compat;
import types.Types.MalType;
import types.Types.*;
import types.MalException;
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
    static function EVAL(ast:MalType, env:Env):MalType {
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
            return EVAL(alst[2], let_env);
        case MalSymbol("do"):
            for (i in 1...alst.length-1)
                EVAL(alst[i], env);
            return EVAL(alst[alst.length-1], env);
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
            switch ( EVAL(alst[0], env)) {
                case MalFunc(f,_,_,_,_,_):
                    var args = alst.slice(1).map(function(x) { return EVAL(x, env); });
                    return f(args);
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
                if (Type.getClass(exc) == MalException) {
                    Compat.println("Error: " + Printer.pr_str(exc.obj, true));
                } else {
                    Compat.println("Error: " + exc);
                };
            }
        }
    }
}
