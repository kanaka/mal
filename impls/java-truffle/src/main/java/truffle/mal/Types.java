package truffle.mal;

import java.util.Iterator;
import java.util.Stack;

import org.organicdesign.fp.collections.PersistentHashMap;
import org.organicdesign.fp.collections.PersistentVector;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;

public class Types {
}

interface MetaHolder<T> {
    Object getMeta();
    T withMeta(Object meta);
}

@SuppressWarnings("serial")
class MalException extends RuntimeException implements TruffleException {
    final Object obj;

    MalException(String message) {
        super(message);
        this.obj = message;
    }

    MalException(Object obj) {
        super(Printer.prStr(obj, true));
        this.obj = obj;
    }

    @Override
    public Throwable fillInStackTrace() {
        return this;
    }

    @Override
    public Node getLocation() {
        return null;
    }
}

abstract class MalValue {
    @Override
    @TruffleBoundary
    public String toString() {
        return Printer.prStr(this, true);
    }
}

@ExportLibrary(InteropLibrary.class)
class MalNil extends MalValue implements TruffleObject {
    public static final MalNil NIL = new MalNil();

    private MalNil() {}

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }
}

@ExportLibrary(InteropLibrary.class)
class MalList extends MalValue implements TruffleObject, Iterable<Object>, MetaHolder<MalList> {
    public static final MalList EMPTY = new MalList();

    @TruffleBoundary
    public static MalList from(Iterable<? extends Object> list) {
        var result = EMPTY;
        var stack = new Stack<Object>();
        list.forEach(stack::add);
        while (!stack.isEmpty()) {
            result = result.cons(stack.pop());
        }
        return result;
    }

    private static int computeHash(Object head, MalList tail) {
        final int prime = 31;
        int result = 1;
        result = prime * result + head.hashCode();
        result = prime * result + tail.hashCode();
        return result;
    }

    public final Object head;
    public final MalList tail;
    private final int hash;
    // The lazy programmer's way of ensuring constant-time size() calls: waste lots of memory!
    public final int length;
    public final Object meta;

    @TruffleBoundary
    private MalList() {
        this.head = null;
        this.tail = null;
        this.hash = 31;
        this.length = 0;
        this.meta = MalNil.NIL;
    }

    @TruffleBoundary
    private MalList(MalList list, Object meta) {
        this.head = list.head;
        this.tail = list.tail;
        this.hash = list.hash;
        this.length = list.length;
        this.meta = meta;
    }

    @TruffleBoundary
    private MalList(Object head, MalList tail, Object meta) {
        this.head = head;
        this.tail = tail;
        this.hash = computeHash(head, tail);
        this.length = tail.length+1;
        this.meta = meta;
    }

    public boolean isEmpty() {
        return head == null;
    }

    @TruffleBoundary
    public MalList cons(Object val) {
        return new MalList(val, this, this.meta);
    }

    @Override
    public int hashCode() {
        return hash;
    }

    @Override
    @TruffleBoundary
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (obj instanceof MalVector) {
            MalVector other = (MalVector)obj;
            if (this.length != other.size())
                return false;
            int i=0;
            MalList list = this;
            while (!list.isEmpty()) {
                if (!list.head.equals(other.get(i))) {
                    return false;
                }
                i++;
                list = list.tail;
            }
            return true;
        }
        if (this.getClass() != obj.getClass())
            return false;

        MalList other = (MalList) obj;
        if (head == null) {
            if (other.head != null)
                return false;
        } else if (!head.equals(other.head))
            return false;
        if (tail == null) {
            if (other.tail != null)
                return false;
        } else if (!tail.equals(other.tail))
            return false;
        return true;
    }

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }

    @Override
    public Iterator<Object> iterator() {
        return new MalListIterator(this);
    }

    private static class MalListIterator implements Iterator<Object> {
        private MalList list;

        MalListIterator(MalList list) {
            this.list = list;
        }

        @Override
        public boolean hasNext() {
            return !list.equals(MalList.EMPTY);
        }

        @Override
        public Object next() {
            Object obj = list.head;
            list = list.tail;
            return obj;
        }
    }

    @Override
    public Object getMeta() {
        return meta;
    }

    @Override
    public MalList withMeta(Object meta) {
        return new MalList(this, meta);
    }
}

@ExportLibrary(InteropLibrary.class)
class MalVector extends MalValue implements TruffleObject, Iterable<Object>, MetaHolder<MalVector> {
    public static final MalVector EMPTY = new MalVector();

    private final PersistentVector<Object> vector;
    private final Object meta;

    private MalVector() {
        vector = PersistentVector.empty();
        meta = MalNil.NIL;
    }

    private MalVector(PersistentVector<Object> vector, Object meta) {
        this.vector = vector;
        this.meta = meta;
    }

    @TruffleBoundary
    public MalVector append(Object obj) {
        return new MalVector(vector.append(obj), this.meta);
    }

    @TruffleBoundary
    public MalVector concat(Object[] objs) {
        var v = vector.mutable();
        for (int i=0; i < objs.length; ++i) {
            v.append(objs[i]);
        }
        return new MalVector(v.immutable(), meta);
    }

    @TruffleBoundary
    public MalVector concat(Iterable<? extends Object> objs) {
        return new MalVector(vector.concat(objs), meta);
    }

    public int size() {
        return vector.size();
    }

    public Object get(int i) {
        return vector.get(i);
    }

    @Override
    public int hashCode() {
        return vector.hashCode();
    }

    @Override
    @TruffleBoundary
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (obj instanceof MalList)
            return obj.equals(this);
        if (getClass() != obj.getClass())
            return false;
        MalVector other = (MalVector) obj;
        return vector.equals(other.vector);
    }

    @Override
    public Iterator<Object> iterator() {
        return vector.iterator();
    }

    @TruffleBoundary
    public MalList toList() {
        MalList result = MalList.EMPTY;
        for (int i=vector.size()-1; i >= 0; i--) {
            result = result.cons(vector.get(i));
        }
        return result;
    }

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }

    @Override
    public Object getMeta() {
        return meta;
    }

    @Override
    public MalVector withMeta(Object meta) {
        return new MalVector(this.vector, meta);
    }
}

@ExportLibrary(InteropLibrary.class)
class MalMap extends MalValue implements TruffleObject, MetaHolder<MalMap> {
    public static final MalMap EMPTY = new MalMap();

    public final PersistentHashMap<Object, Object> map;
    private final Object meta;

    private MalMap() {
        map = PersistentHashMap.EMPTY;
        meta = MalNil.NIL;
    }

    private MalMap(PersistentHashMap<Object,Object> map, Object meta) {
        this.map = map;
        this.meta = meta;
    }

    @TruffleBoundary
    public MalMap assoc(Object key, Object val) {
        return new MalMap(map.assoc(key, val), meta);
    }

    @TruffleBoundary
    public MalMap dissoc(Object key) {
        return new MalMap(map.without(key), meta);
    }

    @TruffleBoundary
    public Object get(Object key) {
        if (map.containsKey(key)) {
            return map.get(key);
        } else {
            return MalNil.NIL;
        }
    }

    @TruffleBoundary
    @Override
    public int hashCode() {
        return map.hashCode();
    }

    @TruffleBoundary
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        MalMap other = (MalMap) obj;
        return map.equals(other.map);
    }

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }

    @Override
    public Object getMeta() {
        return meta;
    }

    @Override
    public MalMap withMeta(Object meta) {
        return new MalMap(map, meta);
    }
}

@ExportLibrary(InteropLibrary.class)
class MalKeyword extends MalValue implements TruffleObject {
    public static final MalKeyword INLINE_Q = MalKeyword.get("inline?");

    public final String keyword;

    public static MalKeyword get(String keyword) {
        return new MalKeyword(keyword);
    }

    private MalKeyword(String keyword) {
        this.keyword = keyword;
    }

    @Override
    public int hashCode() {
        return keyword.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof MalKeyword)) {
            return false;
        }
        return keyword.equals(((MalKeyword)obj).keyword);
    }

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }
}

@ExportLibrary(InteropLibrary.class)
class MalSymbol extends MalValue implements TruffleObject {
    public static MalSymbol get(String symbol) {
        return new MalSymbol(symbol);
    }

    public static final MalSymbol LET_STAR = MalSymbol.get("let*");
    public static final MalSymbol DEF_BANG = MalSymbol.get("def!");
    public static final MalSymbol DO = MalSymbol.get("do");
    public static final MalSymbol IF = MalSymbol.get("if");
    public static final MalSymbol FN_STAR = MalSymbol.get("fn*");
    public static final MalSymbol AMPERSAND = MalSymbol.get("&");
    public static final MalSymbol QUOTE = MalSymbol.get("quote");
    public static final MalSymbol QUASIQUOTE = MalSymbol.get("quasiquote");
    public static final MalSymbol UNQUOTE = MalSymbol.get("unquote");
    public static final MalSymbol SPLICE_UNQUOTE = MalSymbol.get("splice-unquote");
    public static final MalSymbol DEFMACRO = MalSymbol.get("defmacro!");
    public static final MalSymbol MACROEXPAND = MalSymbol.get("macroexpand");
    public static final MalSymbol DEREF = MalSymbol.get("deref");
    public static final MalSymbol TRY = MalSymbol.get("try*");
    public static final MalSymbol CATCH = MalSymbol.get("catch*");

    public final String symbol;

    private MalSymbol(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public int hashCode() {
        return symbol.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        MalSymbol other = (MalSymbol) obj;
        if (symbol == null) {
            if (other.symbol != null)
                return false;
        } else if (!symbol.equals(other.symbol))
            return false;
        return true;
    }

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }
}

@ExportLibrary(InteropLibrary.class)
class MalFunction extends MalValue implements TruffleObject, MetaHolder<MalFunction> {
    final RootCallTarget callTarget;
    final MalEnv closedOverEnv;
    final int numArgs;
    final boolean isMacro;
    final Object meta;
    final boolean canBeTailCalled;

    MalFunction(RootCallTarget callTarget, MalEnv closedOverEnv, int numArgs, boolean canBeTailCalled) {
        this.callTarget = callTarget;
        this.closedOverEnv = closedOverEnv;
        this.numArgs = numArgs;
        this.isMacro = false;
        this.meta = MalNil.NIL;
        this.canBeTailCalled = canBeTailCalled;
    }

    MalFunction(RootCallTarget callTarget, MalEnv closedOverEnv, int numArgs) {
        this(callTarget, closedOverEnv, numArgs, true);
    }

    MalFunction(MalFunction f, boolean isMacro) {
        this(f, f.meta, isMacro, true);
    }

    MalFunction(MalFunction f, Object meta, boolean isMacro) {
        this(f, meta, isMacro, true);
    }

    MalFunction(MalFunction f, Object meta, boolean isMacro, boolean canBeTailCalled) {
        this.callTarget = f.callTarget;
        this.closedOverEnv = f.closedOverEnv;
        this.numArgs = f.numArgs;
        this.isMacro = isMacro;
        this.meta = meta;
        this.canBeTailCalled = canBeTailCalled;
    }

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }

    @Override
    public Object getMeta() {
        return meta;
    }

    @Override
    public MalFunction withMeta(Object meta) {
        return new MalFunction(this, meta, this.isMacro);
    }
}

@ExportLibrary(InteropLibrary.class)
class MalAtom extends MalValue implements TruffleObject {
    private Object value;

    public MalAtom(Object initialValue) {
        this.value = initialValue;
    }

    public Object deref() {
        return value;
    }

    public Object reset(Object newValue) {
        this.value = newValue;
        return newValue;
    }

    @ExportMessage
    Object toDisplayString(boolean allowSideEffects) {
        return this.toString();
    }
}