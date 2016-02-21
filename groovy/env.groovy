import types.MalException
import types.MalSymbol

class env {
    static class Env {
        def data
        def outer

        Env() {
            outer = null
            data = [:]
        }
        Env(Env outer_env) {
            outer = outer_env
            data = [:]
        }
        Env(Env outer_env, binds, exprs) {
            outer = outer_env
            data = [:]
            for (int i=0; i<binds.size; i++) {
                if (binds[i].value == "&") {
                    data[binds[i+1].value] = (exprs.size() > i) ? exprs[i..-1] : []
                    break
                } else {
                    data[binds[i].value] = exprs[i]
                }
            }
        }

        def set(MalSymbol key, def val) {
            data[key.value] = val
        }

        def find(MalSymbol key) {
            if (data.containsKey(key.value)) {
                this
            } else if (outer != null) {
                outer.find(key)
            } else {
                null
            }
        }

        def get(MalSymbol key) {
            def e = find(key)
            if (e == null) {
                throw new MalException("'${key.value}' not found")
            } else {
                e.data.get(key.value)
            }
        }
    }

}

