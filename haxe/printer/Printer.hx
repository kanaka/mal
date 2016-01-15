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
            case MalSymbol(v): v;
            case MalString(v):
                var re1 = ~/\\/g,
                    re2 = ~/"/g,
                    re3 = ~/\n/g;
                //if (haxe.Utf8.charCodeAt(v, 0) == 255) {
                if (v.charAt(0) == "\x7f") {
                    ":" + v.substr(1);
                } else if (_r) {
                    '"' + re3.replace(
                            re2.replace(
                              re1.replace(v, "\\\\"),
                              '\\"'),
                            "\\n") + '"';
                } else {
                    v;
                }
            case MalList(l):
                var lst = l.map(function(e) {return pr_str(e,_r);});
                '(${lst.join(" ")})';
            case MalVector(l):
                var lst = l.map(function(e) {return pr_str(e,_r);});
                '[${lst.join(" ")}]';
            case MalFunc(f,ast,_,params):
                if (ast != null) {
                    '(fn* ${pr_str(params,true)} ${pr_str(ast)})';
                } else {
                    "#<native function>";
                }
            case _: throw "unknown type for printing";
        }
    }
}
