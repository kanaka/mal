package types;

enum MalType {
    MalNil;
    MalTrue;
    MalFalse;
    MalInt(val:Int);
    MalString(val:String);
    MalSymbol(val:String);
    MalList(val:Array<MalType>);
    MalVector(val:Array<MalType>);
}

class Types {
    public static function hello() {
        trace("hello world");
    }
}

