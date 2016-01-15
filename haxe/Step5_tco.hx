import types.Types.MalType;
import types.Types.*;
import reader.*;
import printer.*;
import env.*;
import core.*;

class Step5_tco {
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
            continue; // TCO
        case MalSymbol("do"):
            var el = eval_ast(MalList(alst.slice(1, alst.length-2)), env);
            ast = last(el);
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
            return MalFunc(null, alst[2], env, alst[1]);
        case _:
            var el = eval_ast(ast, env);
            var lst = _list(el);
            switch (first(el)) {
                case MalFunc(f,a,e,params):
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
        #if js
            #error "JS not supported yet"
        #end

        // core.EXT: defined using Haxe
        for (k in Core.ns.keys()) {
            repl_env.set(MalSymbol(k), MalFunc(Core.ns[k],null,null,null));
        }

        // core.mal: defined using the language itself
        rep("(def! not (fn* (a) (if a false true)))");

        while (true) {
            try {
                Sys.print("user> ");
                var line = Sys.stdin().readLine();
                if (line == "") { continue; }
                Sys.println(rep(line));
            } catch (exc:BlankLine) {
                continue;
            } catch (exc:haxe.io.Eof) {
                Sys.exit(0);
            } catch (exc:Dynamic) {
                Sys.println(exc);
            }
        }
    }
}
