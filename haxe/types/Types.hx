package types;

import env.Env;

class MalAtomContainer {
}

enum MalType {
    MalNil;
    MalTrue;
    MalFalse;
    MalInt(val:Int);
    MalString(val:String);
    MalSymbol(val:String);
    MalList(val:Array<MalType>);
    MalVector(val:Array<MalType>);
    MalHashMap(val:Map<String,MalType>);
    MalAtom(val:{val:MalType});
    MalFunc(val:(Array<MalType>)->MalType,
            ast:MalType,
            env:Env,
            params:MalType,
            ismacro:Bool,
            meta:MalType);
}

class Types {
    public static var nil:MalType = MalNil;

    public static function _equal_Q(a:MalType, b:MalType) {
        return switch [a, b] {
            case [MalInt(va), MalInt(vb)]: va == vb;
            case [MalString(va), MalString(vb)] |
                 [MalSymbol(va), MalSymbol(vb)]: va == vb;
            case [MalList(la), MalList(lb)] |
                 [MalList(la), MalVector(lb)] |
                 [MalVector(la), MalList(lb)] |
                 [MalVector(la), MalVector(lb)]:
                if (la.length != lb.length) { return false; }
                for (i in 0...la.length) {
                    if (!_equal_Q(la[i], lb[i])) {
                        false;
                    }
                }
                true;
            case [MalHashMap(ma), MalHashMap(mb)]:
                var maks = ma.keys(),
                    mbks = mb.keys(),
                    malen = 0,
                    mblen = 0;
                for (k in maks) {
                    malen += 1;
                    if ((!mb.exists(k)) || !_equal_Q(ma[k], mb[k])) {
                        return false;
                    }
                }
                for (k in mbks) { mblen += 1; }
                if (malen != mblen) { return false; }
                true;
            case _: a == b;
        }
    }

    public static function _clone(a:MalType) {
        return switch (a) {
            case MalHashMap(m):
                var new_m = new Map<String,MalType>();
                for (k in m.keys()) {
                    new_m[k] = m[k];
                }
                return new_m;
            case _: throw "unsupported clone call";
        }
    }

    public static function _macro_Q(x:MalType) {
        return switch (x) {
            case MalFunc(_,_,_,_,ismacro,_): ismacro;
            case _: false;
        }
    }

    public static function nil_Q(x:MalType) {
        return switch (x) {
            case MalNil: true;
            case _: false;
        }
    }

    public static function true_Q(x:MalType) {
        return switch (x) {
            case MalTrue: true;
            case _: false;
        }
    }

    public static function false_Q(x:MalType) {
        return switch (x) {
            case MalFalse: true;
            case _: false;
        }
    }

    public static function symbol_Q(x:MalType) {
        return switch (x) {
            case MalSymbol(_): true;
            case _: false;
        }
    }

    public static function keyword_Q(x:MalType) {
        return switch (x) {
            case MalString(s):
                s.charAt(0) == "\x7f";
            case _: false;
        }
    }

    // Sequence operations
    public static function list_Q(x:MalType) {
        return switch (x) {
            case MalList(_): true;
            case _: false;
        }
    }

    public static function vector_Q(x:MalType) {
        return switch (x) {
            case MalVector(_): true;
            case _: false;
        }
    }

    public static function first(seq:MalType) {
        return switch (seq) {
            case MalList(l) | MalVector(l):
                if (l.length == 0) { nil; }
                else               { l[0]; }
            case MalNil: MalNil;
            case _: throw "first called on non-sequence";
        }
    }

    public static function rest(seq:MalType) {
        return switch (seq) {
            case MalList(l) | MalVector(l):
                if (l.length <= 1) { MalList([]); }
                else               { MalList(l.slice(1)); }
            case MalNil: MalList([]);
            case _: throw "rest called on non-sequence";
        }
    }

    public static function _nth(seq:MalType, idx:Int) {
        return switch (seq) {
            case MalList(l) | MalVector(l):
                if (l.length > idx) {
                    l[idx];
                } else {
                    throw "nth index out of bounds";
                }
            case _: throw "nth called on non-sequence";
        }
    }

    public static function _list(seq:MalType) {
        return switch (seq) {
            case MalList(l) | MalVector(l): l;
            case _: throw "_array called on non-sequence";
        }
    }

    public static function _map(hm:MalType) {
        return switch (hm) {
            case MalHashMap(m): m;
            case _: throw "_map called on non-hash-map";
        }
    }

    public static function last(seq:MalType) {
        return switch (seq) {
            case MalList(l) | MalVector(l):
                if (l.length == 0) { nil; }
                else               { l[l.length-1]; }
            case _: throw "last called on non-sequence";
        }
    }

    public static function hash_map(kvs:Array<MalType>) {
        var m = new Map<String,MalType>();
        return MalHashMap(assoc_BANG(m, kvs));
    }

    public static function assoc_BANG(m:Map<String,MalType>,
                                      kvs:Array<MalType>) {
        for (i in 0...kvs.length) {
            if (i % 2 > 0) { continue; }
            switch (kvs[i]) {
                case MalString(k):
                    m[k] = kvs[i+1];
                case _: throw "invalid assoc! call";
            }
        }
        return m;
    }

    public static function dissoc_BANG(m:Map<String,MalType>,
                                      ks:Array<MalType>) {
        for (i in 0...ks.length) {
            switch (ks[i]) {
                case MalString(k):
                    m.remove(k);
                case _: throw "invalid dissoc! call";
            }
        }
        return m;
    }

    public static function hash_map_Q(x:MalType) {
        return switch (x) {
            case MalHashMap(_): true;
            case _: false;
        }
    }

    public static function atom_Q(x:MalType) {
        return switch (x) {
            case MalAtom(_): true;
            case _: false;
        }
    }

}

