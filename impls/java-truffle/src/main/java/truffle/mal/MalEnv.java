package truffle.mal;

import java.util.HashMap;
import java.util.Map;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;

class MalEnv {
    final MalEnv outer;
    // bindings is initialized lazily, to avoid the overhead of creating a new HashMap
    // in cases where nothing will be bound (e.g. invoking a function with no arguments)
    private Map<MalSymbol, Object> bindings;

    MalEnv() {
        this.outer = null;
    }

    MalEnv(MalEnv outer) {
        this.outer = outer;
    }

    @TruffleBoundary
    void set(MalSymbol symbol, Object value) {
        if (bindings == null) {
            bindings = new HashMap<>();
        }
        bindings.put(symbol, value);
    }

    @TruffleBoundary
    Object get(MalSymbol symbol) {
        MalEnv env = this;
        while (env != null) {
            if (env.bindings != null) {
                var result = env.bindings.get(symbol);
                if (result != null) {
                    return result;
                }
            }
            env = env.outer;
        }
        return null;
    }
}