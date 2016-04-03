import Compat;
import types.Types.MalType;
import types.Types.*;
import reader.*;
import printer.*;

class Step2_eval {
    // READ
    static function READ(str:String):MalType {
        return Reader.read_str(str);
    }

    // EVAL
    static function eval_ast(ast:MalType, env:Map<String,MalType>) {
        return switch (ast) {
            case MalSymbol(s):
                if (env.exists(s)) {
                    env.get(s);
                } else {
                    throw "'" + s + "' not found";
                }
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

    static function EVAL(ast:MalType, env:Map<String,MalType>):MalType {
        if (!list_Q(ast)) { return eval_ast(ast, env); }

        // apply
        var alst = switch (ast) { case MalList(lst): lst; case _: []; }
        if (alst.length == 0) { return ast; }

        var el = eval_ast(ast, env);
        var lst = switch (el) { case MalList(lst): lst; case _: []; }
        var a0 = lst[0], args = lst.slice(1);
        switch (a0) {
            case MalFunc(f,_,_,_,_,_): return f(args);
            case _: throw "Call of non-function";
        }
    }

    // PRINT
    static function PRINT(exp:MalType):String {
        return Printer.pr_str(exp, true);
    }

    // repl
    static function NumOp(op):MalType {
        return MalFunc(function(args:Array<MalType>) {
            return switch (args) {
                case [MalInt(a), MalInt(b)]: MalInt(op(a,b));
                case _: throw "Invalid numeric op call"; 
            }
            
        },null,null,null,false,nil);
    }
    static var repl_env:Map<String,MalType> = 
        ["+" => NumOp(function(a,b) {return a+b;}),
         "-" => NumOp(function(a,b) {return a-b;}),
         "*" => NumOp(function(a,b) {return a*b;}),
         "/" => NumOp(function(a,b) {return Std.int(a/b);})];

    static function rep(line:String):String {
        return PRINT(EVAL(READ(line), repl_env));
    }

    public static function main() {
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
