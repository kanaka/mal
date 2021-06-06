package truffle.mal;

import java.util.HashMap;
import java.util.Map;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.utilities.UnionAssumption;

import truffle.mal.LexicalScope.EnvSlot;

@ExportLibrary(InteropLibrary.class)
class MalEnv implements TruffleObject {
    final Class<? extends TruffleLanguage<?>> language;
    final MalEnv outer;
    // bindings is initialized lazily, to avoid the overhead of creating a new HashMap
    // in cases where nothing will be bound (e.g. invoking a function with no arguments)
    private Map<MalSymbol, Object> bindings;
    final LexicalScope scope;
    final Object[] staticBindings;
    private Map<MalSymbol, CachedResult> cachedResults;

    private MalEnv(Class<? extends TruffleLanguage<?>> language, MalEnv outer, LexicalScope scope, Object[] staticBindings) {
        this.language = language;
        this.outer = outer;
        this.scope = scope;
        this.staticBindings = staticBindings;
    }

    MalEnv(Class<? extends TruffleLanguage<?>> language) {
        this(language, null, null, null);
    }

    MalEnv(MalEnv outer) {
        this(outer.language, outer, null, null);
    }

    MalEnv(Class<? extends TruffleLanguage<?>> language, LexicalScope scope) {
        this(language, null, scope, new Object[scope.getStaticBindingCount()]);
    }

    MalEnv(MalEnv outer, LexicalScope scope) {
        this(outer.language, outer, scope, new Object[scope.getStaticBindingCount()]);
    }

    /**
     * Dynamic set, for use by def! to bind a symbol that wasn't assigned a slot via a LexicalScope.
     * 
     * @param symbol    the symbol  to bind
     * @param value     its new value
     */
    @TruffleBoundary
    void set(MalSymbol symbol, Object value) {
        if (bindings == null) {
            bindings = new HashMap<>();
        }
        if (!bindings.containsKey(symbol) && scope != null) {
            scope.wasDynamicallyBound(symbol);
        }
        if (cachedResults != null) {
            var result = cachedResults.get(symbol);
            if (result != null) {
                result.notRedefined.invalidate();
            }
        }
        bindings.put(symbol, value);
    }

    /**
     * Bind a symbol that was assigned a slot via a LexicalScope.
     * @param slot      the slot assigned to the symbol
     * @param value     the symbol's new value
     */
    void set(EnvSlot slot, Object value) {
        assert slot.height == 0;
        staticBindings[slot.slotNum] = value;
    }

    /**
     * Dynamic get, for when the looked-up symbol has been assigned a slot
     * but isn't guaranteed to resolve from that lexical scope, e.g. because a def!
     * may have dynamically bound it in an inner scope.
     * 
     * @param symbol
     * @param slot
     * @return
     */
    @TruffleBoundary
    Object get(MalSymbol symbol, EnvSlot slot) {
        var env = this;
        int height = 0;
        while (height < slot.height) {
            Object result = null;
            if (env.bindings != null) {
                result = env.bindings.get(symbol);
            }
            if (result != null) {
                return result;
            }
            env = env.outer;
            height++;
        }
        return env.staticBindings[slot.slotNum];
    }

    /**
     * Dynamic get, for when the looked-up symbol has no statically assigned slot.
     * 
     * @param symbol    the symbol to look up
     * @return          its current value, or null if unbound
     */
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

    @TruffleBoundary
    CachedResult cachedGet(MalSymbol symbol) {
        if (cachedResults == null) {
            cachedResults = new HashMap<>();
        }
        var result = cachedResults.get(symbol);
        if (result == null) {
            Object obj = null;
            if (bindings != null) {
                obj = bindings.get(symbol);
            }
            if (obj == null && outer != null) {
                result = outer.cachedGet(symbol);
            } else {
                result = new CachedResult(obj);
            }
            cachedResults.put(symbol, result);
        }
        return result;
    }

    /**
     * Static get, for when the looked-up symbol is guaranteed to resolve from a particular lexical scope.
     * @param slot
     * @return
     */
    @ExplodeLoop
    Object get(EnvSlot slot) {
        MalEnv env = this;
        for (int i=0; i < slot.height; i++) {
            env = env.outer;
        }
        return env.staticBindings[slot.slotNum];
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
        set(MalSymbol.get(member), value);
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

    static class CachedResult {
        final Object result;
        final Assumption notRedefined = Truffle.getRuntime().createAssumption();

        CachedResult(Object result) {
            this.result = result;
        }
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

/**
 * A LexicalScope tracks the variables known statically to be in a given lexical scope, and keeps track of
 * associated environment slots.
 */
class LexicalScope {
    final LexicalScope parent;
    final int depth;
    final Map<MalSymbol, EnvSlot> slots;
    private int staticBindingCount;
    final Map<MalSymbol, Assumption> notDynamicallyBound;

    LexicalScope() {
        this(null);
    }

    LexicalScope(LexicalScope parent) {
        this.parent = parent;
        this.depth = parent == null? 0 : parent.depth+1;
        this.slots = new HashMap<>();
        this.staticBindingCount = 0;
        this.notDynamicallyBound = new HashMap<>();
    }

    private Assumption getNotDynamicallyBound(MalSymbol symbol) {
        var assumption = notDynamicallyBound.get(symbol);
        if (assumption == null) {
            assumption = Truffle.getRuntime().createAssumption(symbol.symbol+" not dynamically shadowed");
            notDynamicallyBound.put(symbol, assumption);
        }
        return assumption;
    }

    /**
     * Allocate a slot for a symbol in this lexical scope, or return the slot already bound to the symbol.
     * 
     * @param symbol
     * @return
     */
    @TruffleBoundary
    public EnvSlot allocateSlot(MalSymbol symbol) {
        var slot = new EnvSlot(0, slots.size(), getNotDynamicallyBound(symbol));
        slots.put(symbol, slot);
        staticBindingCount++;
        return slot;
    }

    /**
     * If symbols is statically known to be in scope, returns a slot that can be used to look up
     * the bound symbol efficiently. Otherwise, returns null;
     * 
     * @param symbol
     * @return
     */
    @TruffleBoundary
    public EnvSlot getSlot(MalEnv env, MalSymbol symbol) {
        int height = 0;
        var scope = this;
        Assumption assumption = getNotDynamicallyBound(symbol);
        while (scope != null) {
            if (scope.slots.containsKey(symbol)) {
                var slot = scope.slots.get(symbol);
                if (env.get(slot) != null) {
                    if (height == 0) {
                        return slot;
                    } else {
                        return new EnvSlot(height, scope.slots.get(symbol).slotNum, assumption);
                    }
                }
            }
            height++;
            scope = scope.parent;
            env = env.outer;
            if (scope != null) {
                assumption = new UnionAssumption(assumption, scope.getNotDynamicallyBound(symbol));
            }
        }
        return null;
    }

    @TruffleBoundary
    public void wasDynamicallyBound(MalSymbol sym) {
        var assumption = notDynamicallyBound.get(sym);
        if (assumption != null) {
            assumption.invalidate();
        }
    }

    public int getStaticBindingCount() {
        return staticBindingCount;
    }

    static class EnvSlot {
        public final int height;
        public final int slotNum;
        public final Assumption notDynamicallyBound;

        private EnvSlot(int height, int slotNum, Assumption notDynamicallyBound) {
            this.height = height;
            this.slotNum = slotNum;
            this.notDynamicallyBound = notDynamicallyBound;
        }
    }
}