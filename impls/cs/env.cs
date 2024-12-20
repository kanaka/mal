using System.Collections.Generic;
using Mal;
using MalVal = Mal.types.MalVal;
using MalSymbol = Mal.types.MalSymbol;
using MalList = Mal.types.MalList;

namespace Mal {
    public class env {
        public class Env {
            Env outer = null;
            Dictionary<string, MalVal> data = new Dictionary<string, MalVal>();

            public Env(Env outer) {
                this.outer = outer;
            }
            public Env(Env outer, MalList binds, MalList exprs) {
                this.outer = outer;
                for (int i=0; i<binds.size(); i++) {
                    string sym = ((MalSymbol)binds.nth(i)).getName();
                    if (sym == "&") {
                        data[((MalSymbol)binds.nth(i+1)).getName()] = exprs.slice(i);
                        break;
                    } else {
                        data[sym] = exprs.nth(i);
                    }
                }
            }
            
            public MalVal get(string key) {
                if (data.ContainsKey(key)) {
                    return data[key];
                } else if (outer != null) {
                    return outer.get(key);
                } else {
                    return null;
                }
            }

            public Env set(MalSymbol key, MalVal value) {
                data[key.getName()] = value;
                return this;
            }
        }
    }
}
