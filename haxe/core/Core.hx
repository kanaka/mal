package core;

import types.Types.MalType;
import types.Types.*;
import printer.Printer;
import reader.Reader;

class Core {
    static function BoolFn(v) {
        if (v) { return MalTrue; }
        else   { return MalFalse; }
    }

    static function BoolOp(op) {
        return function(args:Array<MalType>) {
            return switch (args) {
                case [MalInt(a), MalInt(b)]: BoolFn(op(a,b));
                case _: throw "Invalid boolean op call"; 
            }
            
        };
    }

    static function NumOp(op) {
        return function(args:Array<MalType>) {
            return switch (args) {
                case [MalInt(a), MalInt(b)]: MalInt(op(a,b));
                case _: throw "Invalid numeric op call"; 
            }
            
        };
    }

    static function equal_Q(args) {
        return BoolFn(_equal_Q(args[0],args[1]));
    }

    static function pr_str(args) {
        return MalString(
            args.map(function(s) { return Printer.pr_str(s,true); }).join(" ")
        );
    }
    static function str(args) {
        return MalString(
            args.map(function(s) { return Printer.pr_str(s,false); }).join("")
        );
    }
    static function prn(args) {
        Sys.println(args.map(function(s) { return Printer.pr_str(s,true); }).join(" "));
        return MalNil;
    }
    static function println(args) {
        Sys.println(args.map(function(s) { return Printer.pr_str(s,false); }).join(" "));
        return MalNil;
    }

    static function read_string(args) {
        return switch (args[0]) {
            case MalString(s): Reader.read_str(s);
            case _: throw "invalid read_str call";
        }
    }

    static function slurp(args) {
        return switch (args[0]) {
            case MalString(s): MalString(sys.io.File.getContent(s));
            case _: throw "invalid slurp call";
        }
    }


    static function empty_Q(args) {
        return switch (args[0]) {
            case MalList(l) | MalVector(l): 
                if (l.length == 0) { MalTrue; }
                else               { MalFalse; }
            case MalNil: MalTrue;
            case _: MalFalse;
        }
    }

    static function count(args) {
        return switch (args[0]) {
            case MalList(l) | MalVector(l): MalInt(l.length);
            case MalNil: MalInt(0);
            case _: throw "count called on non-sequence";
        }
    }


    public static var ns:Map<String,Array<MalType> -> MalType> = [
        "=" => function(a) { return BoolFn(_equal_Q(a[0],a[1])); },

        "pr-str" => pr_str,
        "str" => str,
        "prn" => prn,
        "println" => println,
        "read-string" => read_string,
        "slurp" => slurp,

        "<" => BoolOp(function(a,b) {return a<b;}),
        "<=" => BoolOp(function(a,b) {return a<=b;}),
        ">" => BoolOp(function(a,b) {return a>b;}),
        ">=" => BoolOp(function(a,b) {return a>=b;}),
        "+" => NumOp(function(a,b) {return a+b;}),
        "-" => NumOp(function(a,b) {return a-b;}),
        "*" => NumOp(function(a,b) {return a*b;}),
        "/" => NumOp(function(a,b) {return Std.int(a/b);}),

        "list" => function(a) { return MalList(a); },
        "list?" => function(a) { return BoolFn(list_Q(a[0])); },

        "empty?" => empty_Q,
        "count" => count
    ];
}
