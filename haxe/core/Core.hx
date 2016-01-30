package core;

import Compat;
import types.Types.MalType;
import types.Types.*;
import types.MalException;
import printer.Printer;
import reader.Reader;
import haxe.Timer;

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

    static var start = Timer.stamp();
    static function time_ms(args) {
        return MalInt(Std.int(1000 * (Timer.stamp()-start)));
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
        Compat.println(args.map(function(s) { return Printer.pr_str(s,true); }).join(" "));
        return nil;
    }
    static function println(args) {
        Compat.println(args.map(function(s) { return Printer.pr_str(s,false); }).join(" "));
        return nil;
    }

    static function symbol(args) {
        return switch (args[0]) {
            case MalString(s): MalSymbol(s);
            case MalSymbol(_): args[0];
            case _: throw "Invalid symbol call";
        }
    }

    static function keyword(args) {
        return switch (args[0]) {
            case MalString(s):
                if (keyword_Q(args[0])) {
                    args[0];
                } else {
                    MalString("\x7f" + s);
                }
            case _: throw "Invalid keyword call";
        }
    }

    static function read_string(args) {
        return switch (args[0]) {
            case MalString(s): Reader.read_str(s);
            case _: throw "invalid read_str call";
        }
    }

    static function readline(args) {
        return switch (args[0]) {
            case MalString(prompt):
                try {
                    MalString(Compat.readline(prompt));
                } catch (exc:haxe.io.Eof) {
                    nil;
                }
            case _: throw "invalid readline call";
        }
    }

    static function slurp(args) {
        return switch (args[0]) {
            case MalString(s):
                MalString(Compat.slurp(s));
            case _: throw "invalid slurp call";
        }
    }

    // sequential functions
    static function sequential_Q(args) {
        return BoolFn(list_Q(args[0]) || vector_Q(args[0]));
    }

    static function cons(args) {
        return switch [args[0], args[1]] {
            case [a, MalList(l)] |
                 [a, MalVector(l)]:
                MalList([a].concat(l));
            case [a, MalNil]:
                MalList([a]);
            case _: throw "Invalid cons call";
        }
    }

    static function do_concat(args:Array<MalType>) {
        var res:Array<MalType> = [];
        for (a in args) {
            switch (a) {
                case MalList(l) | MalVector(l): 
                    res = res.concat(l);
                case MalNil:
                    continue;
                case _:
                    throw "concat called with non-sequence";
            }
        }
        return MalList(res);
    }

    static function nth(args) {
        return switch [args[0], args[1]] {
            case [seq, MalInt(idx)]:
                _nth(seq, idx);
            case _: throw "Invalid nth call";
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

    static function apply(args) {
        return switch [args[0], args[args.length-1]] {
            case [MalFunc(f,_,_,_,_), MalList(l)] |
                 [MalFunc(f,_,_,_,_), MalVector(l)]:
                var fargs = args.slice(1,args.length-1).concat(l);
                return f(fargs);
            case _: throw "Invalid apply call";
        }
    }

    static function do_map(args) {
        return switch [args[0], args[1]] {
            case [MalFunc(f,_,_,_,_), MalList(l)] |
                 [MalFunc(f,_,_,_,_), MalVector(l)]:
                return MalList(l.map(function(x) { return f([x]); }));
            case _: throw "Invalid map call";
        }
    }

    static function conj(args) {
        return switch (args[0]) {
            case MalList(l):
                var elems = args.slice(1);
                elems.reverse();
                MalList(elems.concat(l));
            case MalVector(l):
                MalVector(l.concat(args.slice(1)));
            case _: throw "Invalid conj call";
        }
    }


    // hash-map functions

    public static function get(hm:MalType, key:MalType) {
        return switch [hm, key] {
            case [MalHashMap(m), MalString(k)]:
                if (m.exists(k)) {
                    m[k];
                } else {
                    nil;
                }
            case [nil, MalString(k)]:
                nil;
            case _: throw "invalid get call";
        }
    }

    public static function assoc(args) {
        return switch (args[0]) {
            case MalHashMap(m):
                var new_m = _clone(args[0]);
                MalHashMap(assoc_BANG(new_m, args.slice(1)));
            case _: throw "invalid assoc call";
        }
    }

    public static function dissoc(args) {
        return switch (args[0]) {
            case MalHashMap(m):
                var new_m = _clone(args[0]);
                MalHashMap(dissoc_BANG(new_m, args.slice(1)));
            case _: throw "invalid dissoc call";
        }
    }

    public static function contains_Q(hm:MalType, key:MalType) {
        return switch [hm, key] {
            case [MalHashMap(m), MalString(k)]:
                m.exists(k);
            case _: throw "invalid contains? call";
        }
    }

    public static function keys(hm:MalType) {
        return switch (hm) {
            case MalHashMap(m):
                MalList([for (k in m.keys()) MalString(k)]);
            case _: throw "invalid keys call";
        }
    }

    public static function vals(hm:MalType) {
        return switch (hm) {
            case MalHashMap(m):
                MalList([for (k in m.keys()) m[k]]);
            case _: throw "invalid vals call";
        }
    }

    // metadata functions
    static function meta(args) {
        return switch (args[0]) {
            case MalFunc(f,_,_,_,_,meta): meta;
            case _: throw "meta called on non-function";
        }
    }

    static function with_meta(args) {
        return switch (args[0]) {
            case MalFunc(f,a,e,p,mac,_):
                MalFunc(f,a,e,p,mac,args[1]);
            case _: throw "with_meta called on non-function";
        }
    }



    // atom functions

    static function deref(args) {
        return switch (args[0]) {
            case MalAtom(v): v.val;
            case _: throw "deref called on non-atom";
        }
    }

    static function reset_BANG(args) {
        return switch (args[0]) {
            case MalAtom(v): v.val = args[1];
            case _: throw "reset! called on non-atom";
        }
    }

    static function swap_BANG(args) {
        return switch [args[0], args[1]] {
            case [MalAtom(v), MalFunc(f,_,_,_,_)]:
                var fargs = [v.val].concat(args.slice(2));
                v.val = f(fargs);
                v.val;
            case _: throw "swap! called on non-atom";
        }
    }


    public static var ns:Map<String,Array<MalType> -> MalType> = [
        "=" => function(a) { return BoolFn(_equal_Q(a[0],a[1])); },
        "throw" => function(a) { throw new MalException(a[0]); },

        "nil?" => function(a) { return BoolFn(nil_Q(a[0])); },
        "true?" => function(a) { return BoolFn(true_Q(a[0])); },
        "false?" => function(a) { return BoolFn(false_Q(a[0])); },
        "symbol" => symbol,
        "symbol?" => function(a) { return BoolFn(symbol_Q(a[0])); },
        "keyword" => keyword,
        "keyword?" => function(a) { return BoolFn(keyword_Q(a[0])); },

        "pr-str" => pr_str,
        "str" => str,
        "prn" => prn,
        "println" => println,
        "read-string" => read_string,
        "readline" => readline,
        "slurp" => slurp,

        "<" => BoolOp(function(a,b) {return a<b;}),
        "<=" => BoolOp(function(a,b) {return a<=b;}),
        ">" => BoolOp(function(a,b) {return a>b;}),
        ">=" => BoolOp(function(a,b) {return a>=b;}),
        "+" => NumOp(function(a,b) {return a+b;}),
        "-" => NumOp(function(a,b) {return a-b;}),
        "*" => NumOp(function(a,b) {return a*b;}),
        "/" => NumOp(function(a,b) {return Std.int(a/b);}),
        "time-ms" => time_ms,

        "list" => function(a) { return MalList(a); },
        "list?" => function(a) { return BoolFn(list_Q(a[0])); },
        "vector" => function(a) { return MalVector(a); },
        "vector?" => function(a) { return BoolFn(vector_Q(a[0])); },
        "hash-map" => hash_map,
        "map?" => function(a) { return BoolFn(hash_map_Q(a[0])); },
        "assoc" => assoc,
        "dissoc" => dissoc,
        "get" => function(a) { return get(a[0],a[1]); },
        "contains?" => function(a) { return BoolFn(contains_Q(a[0], a[1])); },
        "keys" => function(a) { return keys(a[0]); } ,
        "vals" => function(a) { return vals(a[0]); } ,

        "sequential?" => sequential_Q,
        "cons" => cons,
        "concat" => do_concat,
        "nth" => nth,
        "first" => function(a) { return first(a[0]); },
        "rest" => function(a) { return MalList(_list(a[0]).slice(1)); },
        "empty?" => empty_Q,
        "count" => count,
        "apply" => apply,
        "map" => do_map,

        "conj" => conj,

        "meta" => meta,
        "with-meta" => with_meta,
        "atom" => function(a) { return MalAtom({val:a[0]}); },
        "atom?" => function(a) { return BoolFn(atom_Q(a[0])); },
        "deref" => deref,
        "reset!" => reset_BANG,
        "swap!" => swap_BANG
    ];
}
