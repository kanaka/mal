package env;

import types.Types.MalType;
import types.Types.*;

class Env {
    var data = new Map<String,MalType>();
    var outer:Env = null;
    
    public function new(outer:Env,
                        binds:Array<MalType> = null,
                        exprs:Array<MalType> = null) {
        this.outer = outer;

        if (binds != null) {
            for (i in 0...binds.length) {
                var b = binds[i], e = exprs[i];
                switch (b) {
                    case MalSymbol("&"):
                        switch (binds[i+1]) {
                            case MalSymbol(b2):
                                data[b2] = MalList(exprs.slice(i));
                            case _:
                                throw "invalid vararg binding";
                        }
                        break;
                    case MalSymbol(s):
                        data[s] = e;
                    case _: throw "invalid bind";
                }
            }
        }
    }

    public function set(key:MalType, val:MalType) {
        switch (key) {
            case MalSymbol(s): data[s] = val;
            case _: throw "Invalid Env.set call";
        }
        return val;
    }

    public function get(key:String):MalType {
        if (data.exists(key))   return data.get(key);
        else if (outer != null) return outer.get(key);
        else                    return null;
    }
}
