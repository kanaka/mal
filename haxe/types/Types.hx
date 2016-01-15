package types;

import env.Env;

enum MalType {
    MalNil;
    MalTrue;
    MalFalse;
    MalInt(val:Int);
    MalString(val:String);
    MalSymbol(val:String);
    MalList(val:Array<MalType>);
    MalVector(val:Array<MalType>);
    MalFunc(val:(Array<MalType>)->MalType,
            ast:MalType,
            env:Env,
            params:MalType);
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
            case _: a == b;
        }
    }

    // Sequence operations
    public static function list_Q(x:MalType) {
        return switch (x) {
            case MalList(_): true;
            case _: false;
        }
    }

    public static function first(seq:MalType) {
        return switch (seq) {
            case MalList(l) | MalVector(l):
                if (l.length == 0) { MalNil; }
                else               { l[0]; }
            case _: throw "first called on non-sequence";
        }
    }

    public static function _list(seq:MalType) {
        return switch (seq) {
            case MalList(l) | MalVector(l): l;
            case _: throw "_array called on non-sequence";
        }
    }

    public static function last(seq:MalType) {
        return switch (seq) {
            case MalList(l) | MalVector(l):
                if (l.length == 0) { MalNil; }
                else               { l[l.length-1]; }
            case _: throw "last called on non-sequence";
        }
    }
}

