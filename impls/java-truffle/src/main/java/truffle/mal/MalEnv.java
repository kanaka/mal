package truffle.mal;

import java.util.HashMap;
import java.util.Map;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(InteropLibrary.class)
class MalEnv implements TruffleObject {
    final Class<? extends TruffleLanguage<?>> language;
    final MalEnv outer;
    // bindings is initialized lazily, to avoid the overhead of creating a new HashMap
    // in cases where nothing will be bound (e.g. invoking a function with no arguments)
    private Map<MalSymbol, Object> bindings;

    MalEnv(Class<? extends TruffleLanguage<?>> language) {
        this.language = language;
        this.outer = null;
    }

    MalEnv(MalEnv outer) {
        this.language = outer.language;
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

    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    Class<? extends TruffleLanguage<?>> getLanguage() {
        return language;
    }

    @ExportMessage
    boolean hasMembers() {
        return true;
    }

    @ExportMessage
    @TruffleBoundary
    Object readMember(String member) {
        return bindings.get(MalSymbol.get(member));
    }

    @ExportMessage
    @TruffleBoundary
    boolean isMemberReadable(String member) {
        return bindings.containsKey(MalSymbol.get(member));
    }

    @ExportMessage
    @TruffleBoundary
    Object toDisplayString(boolean allowSideEffects) {
        return "#<environment>";
    }

    @ExportMessage
    @TruffleBoundary
    boolean isMemberInsertable(String member) {
        return !bindings.containsKey(MalSymbol.get(member));
    }

    @ExportMessage
    @TruffleBoundary
    boolean isMemberModifiable(String member) {
        return bindings.containsKey(MalSymbol.get(member));
    }

    @ExportMessage
    @TruffleBoundary
    void writeMember(String member, Object value) {
        this.bindings.put(MalSymbol.get(member), value);
    }

    @ExportMessage
    @TruffleBoundary
    Object getMembers(boolean includeInternal) {
        Object[] names = new Object[bindings.size()];
        int i=0;
        for (MalSymbol sym : bindings.keySet()) {
            names[i++] = sym.symbol;
        }
        return new EnvMembersObject(names);
    }
}

@ExportLibrary(InteropLibrary.class)
final class EnvMembersObject implements TruffleObject {
    private final Object[] names;

    EnvMembersObject(Object[] names) {
        this.names = names;
    }
    @ExportMessage
    boolean hasArrayElements() {
        return true;
    }
    @ExportMessage
    boolean isArrayElementReadable(long index) {
        return index >= 0 && index < names.length;
    }
    @ExportMessage
    long getArraySize() {
        return names.length;
    }
    @ExportMessage
    Object readArrayElement(long index) throws InvalidArrayIndexException {
        if (!isArrayElementReadable(index)) {
            CompilerDirectives.transferToInterpreter();
            throw InvalidArrayIndexException.create(index);
        }
        return names[(int)index];
    }
}