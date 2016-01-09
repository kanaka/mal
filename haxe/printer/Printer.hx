package printer;

import types.Types.MalType;
import types.Types.MalType.*;

class Printer {
    public static function pr_str(exp:MalType, print_readably:Bool = true) {
        var _r = print_readably;
        return switch(exp) {
            case MalNil: "nil";
            case MalTrue: "true";
            case MalFalse: "false";
            case MalInt(v): Std.string(v);
            case MalString(v):
                if (_r) {
                    '"' + v + '"';
                } else {
                    v;
                }
            case MalSymbol(v): Std.string(v);
            case MalList(l):
                var lst = l.map(function(e) {return pr_str(e,_r);});
                "(" + lst.join(" ") + ")";
            case MalVector(l):
                var lst = l.map(function(e) {return pr_str(e,_r);});
                "[" + lst.join(" ") + "]";
            case _: throw "unknown type for printing";
        }
    }
}
