package mal;

import java.util.HashMap;

import mal.types.MalThrowable;
import mal.types.MalException;
import mal.types.MalVal;
import mal.types.MalSymbol;
import mal.types.MalList;

public class env {
    public static class Env {
        Env outer = null;
        HashMap<String,MalVal> data = new HashMap<String,MalVal>();

        public Env(Env outer) {
            this.outer = outer;
        }
        public Env(Env outer, MalList binds, MalList exprs) {
            this.outer = outer;
            for (Integer i=0; i<binds.size(); i++) {
                String sym = ((MalSymbol)binds.nth(i)).getName();
                if (sym.equals("&")) {
                    data.put(((MalSymbol)binds.nth(i+1)).getName(),
                            exprs.slice(i));
                    break;
                } else {
                    data.put(sym, exprs.nth(i));
                }
            }
        }
        
        public Env find(String key) {
            if (data.containsKey(key)) {
                return this;
            } else if (outer != null) {
                return outer.find(key);
            } else {
                return null;
            }
        }

        public MalVal get(String key) throws MalThrowable {
            Env e = find(key);
            if (e == null) {
                throw new MalException("'" + key + "' not found");
            } else {
                return e.data.get(key);
            }
        }

        public Env set(String key, MalVal value) {
            data.put(key, value);
            return this;
        }
    }
}
