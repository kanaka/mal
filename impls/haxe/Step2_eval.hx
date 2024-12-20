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
    static function EVAL(ast:MalType, env:Map<String,MalType>) {
        // Compat.println("EVAL: " + PRINT(ast));
        var alst;
        switch (ast) {
            case MalSymbol(s):
                if (env.exists(s)) {
                    return env.get(s);
                } else {
                    throw "'" + s + "' not found";
                }
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
        switch ( EVAL(alst[0], env)) {
            case MalFunc(f,_,_,_,_,_):
                var args = alst.slice(1).map(function(x) { return EVAL(x, env); });
                return f(args);
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
                Compat.println("Error: " + exc);
            }
        }
    }
}
