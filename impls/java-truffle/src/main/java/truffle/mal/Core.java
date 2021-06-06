package truffle.mal;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

class Core {
    static final Map<String, NodeFactory<? extends BuiltinNode>> NS = new HashMap<>();

    static {
        NS.put("+", AddBuiltinFactory.getInstance());
        NS.put("-", SubtractBuiltinFactory.getInstance());
        NS.put("*", MultiplyBuiltinFactory.getInstance());
        NS.put("/", DivideBuiltinFactory.getInstance());

        NS.put("prn", PrnBuiltinFactory.getInstance());
        NS.put("list", ListBuiltinFactory.getInstance());
        NS.put("list?", IsListBuiltinFactory.getInstance());
        NS.put("empty?", IsEmptyBuiltinFactory.getInstance());
        NS.put("count", CountBuiltinFactory.getInstance());
        NS.put("=", EqualsBuiltinFactory.getInstance());
        NS.put("<", LessThanBuiltinFactory.getInstance());
        NS.put("<=", LessThanEqualBuiltinFactory.getInstance());
        NS.put(">", GreaterThanBuiltinFactory.getInstance());
        NS.put(">=", GreaterThanEqualBuiltinFactory.getInstance());
        NS.put("pr-str", PrStrBuiltinFactory.getInstance());
        NS.put("str", StrBuiltinFactory.getInstance());
        NS.put("println", PrintlnBuiltinFactory.getInstance());

        NS.put("read-string", ReadStringBuiltinFactory.getInstance());
        NS.put("slurp", SlurpBuiltinFactory.getInstance());
        NS.put("eval", EvalBuiltinFactory.getInstance());
        NS.put("atom", AtomBuiltinFactory.getInstance());
        NS.put("atom?", IsAtomBuiltinFactory.getInstance());
        NS.put("deref", DerefBuiltinFactory.getInstance());
        NS.put("reset!", ResetBuiltinFactory.getInstance());
        NS.put("swap!", SwapBuiltinFactory.getInstance());

        NS.put("cons", ConsBuiltinFactory.getInstance());
        NS.put("concat", ConcatBuiltinFactory.getInstance());
        NS.put("vec", VecBuiltinFactory.getInstance());

        NS.put("nth", NthBuiltinFactory.getInstance());
        NS.put("first", FirstBuiltinFactory.getInstance());
        NS.put("rest", RestBuiltinFactory.getInstance());

        NS.put("throw", ThrowBuiltinFactory.getInstance());
        NS.put("apply", ApplyBuiltinFactory.getInstance());
        NS.put("map", MapBuiltinFactory.getInstance());
        NS.put("nil?", IsNilBuiltinFactory.getInstance());
        NS.put("true?", IsTrueBuiltinFactory.getInstance());
        NS.put("false?", IsFalseBuiltinFactory.getInstance());
        NS.put("symbol?", IsSymbolBuiltinFactory.getInstance());
        NS.put("symbol", SymbolBuiltinFactory.getInstance());
        NS.put("keyword", KeywordBuiltinFactory.getInstance());
        NS.put("keyword?", IsKeywordBuiltinFactory.getInstance());
        NS.put("vector", VectorBuiltinFactory.getInstance());
        NS.put("vector?", IsVectorBuiltinFactory.getInstance());
        NS.put("sequential?", IsSequentialBuiltinFactory.getInstance());
        NS.put("hash-map", HashMapBuiltinFactory.getInstance());
        NS.put("map?", IsMapBuiltinFactory.getInstance());
        NS.put("assoc", AssocBuiltinFactory.getInstance());
        NS.put("dissoc", DissocBuiltinFactory.getInstance());
        NS.put("get", GetBuiltinFactory.getInstance());
        NS.put("contains?", ContainsBuiltinFactory.getInstance());
        NS.put("keys", KeysBuiltinFactory.getInstance());
        NS.put("vals", ValsBuiltinFactory.getInstance());

        NS.put("readline", ReadlineBuiltinFactory.getInstance());
        NS.put("meta", MetaBuiltinFactory.getInstance());
        NS.put("with-meta", WithMetaBuiltinFactory.getInstance());
        NS.put("time-ms", TimeMsBuiltinFactory.getInstance());
        NS.put("conj", ConjBuiltinFactory.getInstance());
        NS.put("string?", IsStringBuiltinFactory.getInstance());
        NS.put("number?", IsNumberBuiltinFactory.getInstance());
        NS.put("fn?", IsFnBuiltinFactory.getInstance());
        NS.put("macro?", IsMacroBuiltinFactory.getInstance());
        NS.put("seq", SeqBuiltinFactory.getInstance());
    }

    static MalEnv newGlobalEnv(Class<? extends TruffleLanguage<?>> languageClass, TruffleLanguage<?> language) {
        var env = new MalEnv(languageClass);
        for (var entry : NS.entrySet()) {
            var root = new BuiltinRootNode(language, entry.getValue());
            var fnVal = new MalFunction(
                    Truffle.getRuntime().createCallTarget(root), null, root.getNumArgs(),
                    // Built-in functions should not be tail called. It doesn't help with
                    // stack consumption, since they aren't recursive, and it *does*
                    // invalidate direct call sites, which hurts performance.
                    false);
            env.set(MalSymbol.get(entry.getKey()), fnVal);
        }
        return env;
    }
}

abstract class AbstractInvokeNode extends Node {
    abstract Object invoke(CallTarget target, Object[] args);
}
/** A hack to make certain nodes sharable across languages.
 */
interface IMalLanguage {
    CallTarget evalForm(Object form);
    AbstractInvokeNode invokeNode();
    PrintStream out();
    BufferedReader in();
}

abstract class BuiltinNode extends Node {
    protected IMalLanguage language;

    protected void setLanguage(IMalLanguage language) {
        this.language = language;
    }

    @TruffleBoundary
    protected static MalException illegalArgumentException(String expectedType, Object obj) {
        return new MalException("Illegal argument: '"+obj.toString()+"' is not of type "+expectedType);
    }

    final String name;

    protected BuiltinNode(String name) {
        this.name = name;
    }

    abstract Object executeGeneric(VirtualFrame frame);

    long executeLong(VirtualFrame frame) throws UnexpectedResultException {
        var value = executeGeneric(frame);
        if (value instanceof Long) {
            return (long)value;
        }
        throw new UnexpectedResultException(value);
    }

    boolean executeBoolean(VirtualFrame frame) throws UnexpectedResultException {
        var value = executeGeneric(frame);
        if (value instanceof Boolean) {
            return (boolean)value;
        }
        throw new UnexpectedResultException(value);
    }
}

class ReadArgNode extends Node {
    final int argNum;

    ReadArgNode(int argNum) {
        this.argNum = argNum;
    }

    Object executeGeneric(VirtualFrame frame) {
        return frame.getArguments()[argNum];
    }
}

class ReadArgsNode extends Node {
    final int argPos;

    ReadArgsNode(int argPos) {
        this.argPos = argPos;
    }

    Object executeGeneric(VirtualFrame frame) {
        Object[] args = frame.getArguments();
        final var len = args.length - argPos;
        var result = new Object[len];
        System.arraycopy(args, argPos, result, 0, len);
        return result;
    }
}

class BuiltinRootNode extends RootNode {
    private final int numArgs;
    @Child private BuiltinNode node;

    public BuiltinRootNode(TruffleLanguage<?> lang, NodeFactory<? extends BuiltinNode> nodeFactory) {
        super(lang);
        var sig = nodeFactory.getExecutionSignature();
        int numArgs = nodeFactory.getExecutionSignature().size();
        Object[] readArgNodes = new Node[numArgs];
        for (int i=0; i < numArgs; ++i) {
            if (sig.get(i).equals(ReadArgsNode.class)) {
                assert i == numArgs-1 : "ReadArgsNode must be last argument";
                readArgNodes[i] = new ReadArgsNode(i+1);
                numArgs = -1; // variadic
            } else {
                readArgNodes[i] = new ReadArgNode(i+1);
            }
        }
        node = nodeFactory.createNode(readArgNodes);
        if (lang instanceof IMalLanguage) {
            node.setLanguage((IMalLanguage)lang);
        }
        this.numArgs = numArgs;
    }

    public int getNumArgs() {
        return numArgs;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return node.executeGeneric(frame);
    }

    @Override
    public String toString() {
        return "#<builtin "+node.name+">";
    }
}

/************** MATH *******************/

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class AddBuiltin extends BuiltinNode {

    protected AddBuiltin() { super("+"); }

    @Specialization
    protected long add(long lhs, long rhs) {
        return lhs + rhs;
    }
}

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class SubtractBuiltin extends BuiltinNode {

    protected SubtractBuiltin() { super("-"); }

    @Specialization
    protected long subtract(long lhs, long rhs) {
        return lhs - rhs;
    }

}

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class MultiplyBuiltin extends BuiltinNode {

    protected MultiplyBuiltin() { super("*"); }

    @Specialization
    protected long multiply(long lhs, long rhs) {
        return lhs * rhs;
    }
}

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class DivideBuiltin extends BuiltinNode {
    protected DivideBuiltin() { super("/"); }

    @Specialization
    protected long divide(long lhs, long rhs) {
        return lhs / rhs;
    }
}

/************** STRINGS *******************/

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class PrnBuiltin extends BuiltinNode {
    protected PrnBuiltin() { super("prn"); }

    @Specialization
    @TruffleBoundary
    protected Object prn(Object[] args) {
        var buf = new StringBuilder();
        if (args.length > 0) {
            Printer.prStr(buf, args[0], true);
        }
        for (int i=1; i < args.length; ++i) {
            buf.append(' ');
            Printer.prStr(buf, args[i], true);
        }
        language.out().println(buf.toString());
        return MalNil.NIL;
    }
}

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class PrStrBuiltin extends BuiltinNode {

    protected PrStrBuiltin() { super("pr-str"); }

    @Specialization
    @TruffleBoundary
    protected String prStr(Object... args) {
        var buf = new StringBuilder();
        if (args.length > 0) {
            Printer.prStr(buf, args[0], true);
        }
        for (int i=1; i < args.length; ++i) {
            buf.append(' ');
            Printer.prStr(buf, args[i], true);
        }
        return buf.toString();
    }
}

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class StrBuiltin extends BuiltinNode {

    protected StrBuiltin() { super("str"); }

    @Specialization
    @TruffleBoundary
    protected String prStr(Object... args) {
        var buf = new StringBuilder();
        for (int i=0; i < args.length; ++i) {
            Printer.prStr(buf, args[i], false);
        }
        return buf.toString();
    }
}

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class PrintlnBuiltin extends BuiltinNode {

    protected PrintlnBuiltin() { super("println"); }

    @Specialization
    @TruffleBoundary
    protected MalNil println(Object... args) {
        var buf = new StringBuilder();
        if (args.length > 0) {
            Printer.prStr(buf, args[0], false);
        }
        for (int i=1; i < args.length; ++i) {
            buf.append(' ');
            Printer.prStr(buf, args[i], false);
        }
        // The correct thing is to use the output stream associated with our language context.
        // However, since each step is effectively its own language, and we wish
        // to share this node among them, we'll just cheat and call System.out directly.
        language.out().println(buf.toString());
        return MalNil.NIL;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class ReadStringBuiltin extends BuiltinNode {

    protected ReadStringBuiltin() { super("read-string"); }

    @TruffleBoundary
    @Specialization
    protected Object readString(String s) {
        return Reader.readStr(s);
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class SlurpBuiltin extends BuiltinNode {

    protected SlurpBuiltin() { super("slurp"); }

    @TruffleBoundary
    @Specialization
    protected String slurp(String path) {
        try {
            var writer = new StringWriter();
            var reader = new InputStreamReader(new FileInputStream(path));
            try {
                reader.transferTo(writer);
                return writer.toString();
            } finally {
                reader.close();
            }
        } catch (FileNotFoundException ex) {
            throw new MalException(ex.getMessage());
        } catch (IOException ex) {
            throw new MalException(ex.getMessage());
        }
    }
}

/************ COLLECTIONS *****************/

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class ListBuiltin extends BuiltinNode {

    protected ListBuiltin() { super("list"); }

    @Specialization
    protected MalList list(Object[] args) {
        var result = MalList.EMPTY;
        for (int i=args.length-1; i >= 0; --i) {
            result = result.cons(args[i]);
        }
        return result;
    }
}

@NodeChild(value = "list", type = ReadArgNode.class)
@GenerateNodeFactory
abstract class IsListBuiltin extends BuiltinNode {

    protected IsListBuiltin() { super("list?"); }

    @Specialization
    public boolean isList(MalList list) {
        return true;
    }

    @Fallback
    public boolean isList(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsEmptyBuiltin extends BuiltinNode {

    protected IsEmptyBuiltin() { super("empty?"); }

    @Specialization
    protected boolean isEmpty(MalList list) {
        return list.head == null;
    }

    @Specialization
    protected boolean isEmpty(MalVector vector) {
        return vector.size() == 0;
    }

    @Fallback
    protected Object typeError(Object arg) {
        throw illegalArgumentException("list", arg);
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class CountBuiltin extends BuiltinNode {

    protected CountBuiltin() { super("count"); }

    @Specialization
    protected long count(MalList arg) {
        return arg.length;
    }

    @Specialization
    protected long count(MalVector arg) {
        return arg.size();
    }

    @Specialization
    protected long count(MalNil arg) {
        return 0;
    }

    @Fallback
    protected Object count(Object arg) {
        throw illegalArgumentException("list", arg);
    }
}

@NodeChild(value="obj", type=ReadArgNode.class)
@NodeChild(value="list", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class ConsBuiltin extends BuiltinNode {

    protected ConsBuiltin() { super("cons"); }

    @Specialization
    @TruffleBoundary
    protected MalList cons(Object obj, MalVector vec) {
        return cons(obj, vec.toList());
    }

    @Specialization
    @TruffleBoundary
    protected MalList cons(Object obj, MalList list) {
        return list.cons(obj);
    }
}

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class ConcatBuiltin extends BuiltinNode {

    protected ConcatBuiltin() { super("concat"); }

    private MalList concat1(MalList a, MalList b) {
        var elems = new Stack<Object>();
        for (Object elem : a) {
            elems.push(elem);
        }
        while (!elems.isEmpty()) {
            b = b.cons(elems.pop());
        }
        return b;
    }

    private MalList concat1(MalVector a, MalList b) {
        for (int i=a.size()-1; i >= 0; i--) {
            b = b.cons(a.get(i));
        }
        return b;
    }

    @Specialization
    @TruffleBoundary
    protected MalList concat(Object... args) {
        if (args.length == 0) {
            return MalList.EMPTY;
        }
        Object arg = args[args.length-1];
        MalList result;
        if (arg instanceof MalVector) {
            result = ((MalVector) arg).toList();
        } else {
            result = (MalList)arg;
        }
        for (int i=args.length-2; i >= 0; --i) {
            arg = args[i];
            if (arg instanceof MalVector) {
                result = concat1((MalVector)arg, result);
            } else {
                result = concat1((MalList)arg, result);
            }
        }
        return result;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class VecBuiltin extends BuiltinNode {

    protected VecBuiltin() { super("vec"); }
    
    @Specialization
    protected MalVector vec(MalVector v) {
        return v;
    }

    @Specialization
    protected MalVector vec(MalList l) {
        return MalVector.EMPTY.concat(l);
    }
}

@NodeChild(value="list", type=ReadArgNode.class)
@NodeChild(value="n", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class NthBuiltin extends BuiltinNode {

    protected NthBuiltin() { super("nth"); }

    @Specialization
    @TruffleBoundary
    protected Object nth(MalVector vec, long n) {
        if (n >= vec.size()) {
            throwInvalidArgument();
        }
        return vec.get((int)n);
    }

    private void throwInvalidArgument() {
        throw new MalException("Out of bounds");
    }

    @Specialization
    protected Object nth(MalList list, long n) {
        if (n >= list.length) {
            throwInvalidArgument();
        }
        while (--n >= 0) {
            list = list.tail;
        }
        return list.head;
    }
}

@GenerateNodeFactory
@NodeChild(value="arg", type=ReadArgNode.class)
abstract class FirstBuiltin extends BuiltinNode {
    protected FirstBuiltin() { super("first"); }

    @Specialization
    protected MalNil first(MalNil nil) {
        return MalNil.NIL;
    }

    @Specialization
    protected Object first(MalVector vec) {
        if (vec.size() == 0)
            return MalNil.NIL;
        return vec.get(0);
    }

    @Specialization
    protected Object first(MalList list) {
        if (list.head == null) {
            return MalNil.NIL;
        }
        return list.head;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class RestBuiltin extends BuiltinNode {

    protected RestBuiltin() { super("rest"); }

    @Specialization
    protected MalList rest(MalNil nil) {
        return MalList.EMPTY;
    }

    @Specialization
    @TruffleBoundary
    protected MalList rest(MalVector vec) {
        return rest(vec.toList());
    }

    @Specialization
    protected MalList rest(MalList list) {
        if (list.head == null) {
            return list;
        }
        return list.tail;
    }
}

@NodeChild(value="fn", type=ReadArgNode.class)
@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class ApplyBuiltin extends BuiltinNode {
    @Child private AbstractInvokeNode invokeNode;

    protected ApplyBuiltin() {
        super("apply");
    }

    @Override
    protected void setLanguage(IMalLanguage language) {
        super.setLanguage(language);
        this.invokeNode = language.invokeNode();
    }

    @TruffleBoundary
    private Object[] getArgs(Object[] args) {
        Object[] fnArgs;
        if (args.length == 0) {
            fnArgs = args;
        } else {
            Object lastArg = args[args.length-1];
            int lastArgSize;
            if (lastArg instanceof MalVector) {
                lastArgSize = ((MalVector)lastArg).size();
            } else {
                lastArgSize = (int)((MalList)lastArg).length;
            }
            fnArgs = new Object[args.length + lastArgSize];
            for (int i=0; i < args.length-1; i++) {
                fnArgs[i+1] = args[i];
            }
            int i = args.length;
            assert lastArg instanceof Iterable<?>;
            for (Object obj : ((Iterable<?>)lastArg)) {
                fnArgs[i++] = obj;
            }
        }
        return fnArgs;
    }

    @Specialization
    protected Object apply(VirtualFrame frame, MalFunction fn, Object[] args) {
        var fnArgs = getArgs(args);
        fnArgs[0] = fn.closedOverEnv;
        return invokeNode.invoke(fn.callTarget, fnArgs);
    }
}

@NodeChild(value="fn", type=ReadArgNode.class)
@NodeChild(value="col", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class MapBuiltin extends BuiltinNode {
    @Child private AbstractInvokeNode invokeNode;

    protected MapBuiltin() {
        super("map");
    }

    @Override
    protected void setLanguage(IMalLanguage language) {
        super.setLanguage(language);
        invokeNode = language.invokeNode();
    }

    @TruffleBoundary
    private Object doMap(MalFunction fn, Iterable<Object> vals) {
        var result = new ArrayList<Object>();
        Object[] args = new Object[2];
        args[0] = fn.closedOverEnv;
        for (Object obj : vals) {
            args[1] = obj;
            result.add(invokeNode.invoke(fn.callTarget, args));
        }
        return MalList.from(result);
    }

    @Specialization
    protected Object map(MalFunction fn, MalVector vec) {
        return doMap(fn, vec);
    }

    @Specialization
    protected Object map(MalFunction fn, MalList list) {
        return doMap(fn, list);
    }
}

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class VectorBuiltin extends BuiltinNode {

    protected VectorBuiltin() { super("vector"); }

    @TruffleBoundary
    @Specialization
    public MalVector vector(Object[] args) {
        MalVector v = MalVector.EMPTY;
        for (Object arg : args) {
            v = v.append(arg);
        }
        return v;
    }
}

@NodeChild(value="col", type=ReadArgNode.class)
@NodeChild(value="elems", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class ConjBuiltin extends BuiltinNode {

    protected ConjBuiltin() { super("conj"); }

    @Specialization
    protected MalList conj(MalList list, Object[] elems) {
        for (int i=0; i < elems.length; i++) {
            list = list.cons(elems[i]);
        }
        return list;
    }

    @Specialization
    protected MalVector conj(MalVector vec, Object[] elems) {
        for (int i=0; i < elems.length; i++) {
            vec = vec.append(elems[i]);
        }
        return vec;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class SeqBuiltin extends BuiltinNode {

    protected SeqBuiltin() { super("seq"); }

    @Specialization
    protected Object seq(MalList list) {
        if (list.length == 0) {
            return MalNil.NIL;
        }
        return list;
    }
    @Specialization
    protected Object seq(MalVector vec) {
        if (vec.size() == 0) {
            return MalNil.NIL;
        }
        return vec.toList();
    }
    @Specialization
    protected Object seq(String str) {
        if (str.isEmpty()) {
            return MalNil.NIL;
        }
        MalList l = MalList.EMPTY;
        for (int i=str.length()-1; i >= 0; i--) {
            l = l.cons(str.substring(i, i+1));
        }
        return l;
    }
    @Specialization
    protected MalNil seq(MalNil nil) {
        return nil;
    }
}

/************* Maps ********************/

@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class HashMapBuiltin extends BuiltinNode {

    protected HashMapBuiltin() { super("hash-map"); }

    @Specialization
    @TruffleBoundary
    protected MalMap hashMap(Object[] args) {
        MalMap map = MalMap.EMPTY;
        for (int i=0; i < args.length; i += 2) {
            map = map.assoc(args[i], args[i+1]);
        }
        return map;
    }
}

@NodeChild(value="map", type=ReadArgNode.class)
@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class AssocBuiltin extends BuiltinNode {

    protected AssocBuiltin() { super("assoc"); }

    @Specialization
    protected Object assoc(MalMap map, Object[] args) {
        for (int i=0; i < args.length; i+=2) {
            map = map.assoc(args[i], args[i+1]);
        }
        return map;
    }
}

@NodeChild(value="map", type=ReadArgNode.class)
@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class DissocBuiltin extends BuiltinNode {

    protected DissocBuiltin() { super("dissoc"); }

    @Specialization
    protected MalMap dissoc(MalMap map, Object[] args) {
        for (Object arg : args) {
            map = map.dissoc(arg);
        }
        return map;
    }
}

@NodeChild(value="map", type=ReadArgNode.class)
@NodeChild(value="key", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class GetBuiltin extends BuiltinNode {

    protected GetBuiltin() { super("get"); }

    @Specialization
    @TruffleBoundary
    protected Object get(MalMap map, Object key) {
        return map.map.getOrDefault(key, MalNil.NIL);
    }

    @Specialization
    protected Object get(MalNil nil, Object key) {
        return MalNil.NIL;
    }
}

@NodeChild(value="map", type=ReadArgNode.class)
@NodeChild(value="key", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class ContainsBuiltin extends BuiltinNode {

    protected ContainsBuiltin() { super("contains?"); }

    @Specialization
    @TruffleBoundary
    protected boolean contains(MalMap map, Object key) {
        return map.map.containsKey(key);
    }
}

@NodeChild(value="map", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class KeysBuiltin extends BuiltinNode {

    protected KeysBuiltin() { super("keys"); }

    @Specialization
    @TruffleBoundary
    protected MalList keys(MalMap map) {
        MalList list = MalList.EMPTY;
        var iter = map.map.keyIterator();
        while (iter.hasNext()) {
            list = list.cons(iter.next());
        }
        return list;
    }
}

@NodeChild(value="map", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class ValsBuiltin extends BuiltinNode {

    protected ValsBuiltin() { super("vals"); }

    @Specialization
    @TruffleBoundary
    protected Object vals(MalMap map) {
        MalList list = MalList.EMPTY;
        var iter = map.map.valIterator();
        while (iter.hasNext()) {
            list = list.cons(iter.next());
        }
        return list;
    }
}

/************* COMPARISONS *************/

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class EqualsBuiltin extends BuiltinNode {

    protected EqualsBuiltin() { super("="); }

    @Specialization
    protected boolean equals(long lhs, long rhs) {
        return lhs == rhs;
    }

    @Specialization
    protected boolean equals(boolean lhs, boolean rhs) {
        return lhs == rhs;
    }

    @TruffleBoundary
    @Specialization
    protected boolean equals(String lhs, String rhs) {
        return lhs.equals(rhs);
    }

    @Specialization
    protected boolean equals(MalFunction lhs, MalFunction rhs) {
        return lhs == rhs;
    }

    @Specialization
    protected boolean equals(MalNil lhs, MalNil rhs) {
        return lhs == rhs;
    }

    @TruffleBoundary
    @Specialization
    protected boolean equals(MalValue lhs, MalValue rhs) {
        if (lhs == null) {
            return lhs == rhs;
        } else {
            return lhs.equals(rhs);
        }
    }

    @Fallback
    protected boolean equals(Object lhs, Object rhs) {
        return false;
    }
}

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class GreaterThanBuiltin extends BuiltinNode {

    protected GreaterThanBuiltin() { super(">"); }

    @Specialization
    protected boolean greaterThan(long lhs, long rhs) {
        return lhs > rhs;
    }

    @Specialization
    protected Object typeError(Object lhs, long rhs) {
        throw illegalArgumentException("integer", lhs);
    }

    @Fallback
    protected Object typeError(Object lhs, Object rhs) {
        throw illegalArgumentException("integer", rhs);
    }
}

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class GreaterThanEqualBuiltin extends BuiltinNode {

    protected GreaterThanEqualBuiltin() { super(">="); }

    @Specialization
    protected boolean greaterThanEqual(long lhs, long rhs) {
        return lhs >= rhs;
    }

    @Specialization
    protected Object typeError(Object lhs, long rhs) {
        throw illegalArgumentException("integer", lhs);
    }

    @Fallback
    protected Object typeError(Object lhs, Object rhs) {
        throw illegalArgumentException("integer", rhs);
    }
}

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class LessThanBuiltin extends BuiltinNode {

    protected LessThanBuiltin() { super("<"); }

    @Specialization
    protected boolean lessThan(long lhs, long rhs) {
        return lhs < rhs;
    }

    @Specialization
    protected Object typeError(Object lhs, long rhs) {
        throw illegalArgumentException("integer", lhs);
    }

    @Fallback
    protected Object typeError(Object lhs, Object rhs) {
        throw illegalArgumentException("integer", rhs);
    }
}

@NodeChild(value="lhs", type=ReadArgNode.class)
@NodeChild(value="rhs", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class LessThanEqualBuiltin extends BuiltinNode {

    protected LessThanEqualBuiltin() { super("<="); }

    @Specialization
    protected boolean lessThanEqual(long lhs, long rhs) {
        return lhs <= rhs;
    }

    @Specialization
    protected Object typeError(Object lhs, long rhs) {
        throw illegalArgumentException("integer", lhs);
    }

    @Fallback
    protected Object typeError(Object lhs, Object rhs) {
        throw illegalArgumentException("integer", rhs);
    }
}

/*************** Atoms ********************/

@NodeChild(value="val", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class AtomBuiltin extends BuiltinNode {
    protected AtomBuiltin() { super("atom"); }

    @Specialization
    protected MalAtom atom(Object val) {
        return new MalAtom(val);
    }
}

@NodeChild(value="val", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsAtomBuiltin extends BuiltinNode {

    protected IsAtomBuiltin() { super("atom?"); }

    @Specialization
    protected boolean isAtom(Object obj) {
        return obj instanceof MalAtom;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class DerefBuiltin extends BuiltinNode {

    protected DerefBuiltin() { super("deref"); }

    @Specialization
    protected Object deref(MalAtom atom) {
        return atom.deref();
    }
}

@NodeChild(value="atom", type=ReadArgNode.class)
@NodeChild(value="val", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class ResetBuiltin extends BuiltinNode {

    protected ResetBuiltin() { super("reset!"); }

    @Specialization
    protected Object reset(MalAtom atom, Object val) {
        atom.reset(val);
        return val;
    }
}

@NodeChild(value="atom", type=ReadArgNode.class)
@NodeChild(value="fn", type=ReadArgNode.class)
@NodeChild(value="args", type=ReadArgsNode.class)
@GenerateNodeFactory
abstract class SwapBuiltin extends BuiltinNode {
    @Child private AbstractInvokeNode invokeNode;

    protected SwapBuiltin() {
        super("swap!");
    }

    @Override
    protected void setLanguage(IMalLanguage language) {
        super.setLanguage(language);
        this.invokeNode = language.invokeNode();
    }

    @Specialization
    protected Object swap(MalAtom atom, MalFunction fn, Object... args) {
        synchronized (atom) {
            Object[] fnArgs = new Object[2+args.length];
            fnArgs[0] = fn.closedOverEnv;
            fnArgs[1] = atom.deref();
            for (int i=0; i < args.length; i++) {
                fnArgs[i+2] = args[i];
            }
            Object newVal = invokeNode.invoke(fn.callTarget, fnArgs);
            atom.reset(newVal);
            return newVal;
        }
    }
}

/*************** Predicates ***************/

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsNilBuiltin extends BuiltinNode {
    protected IsNilBuiltin() { super("nil?"); }

    @Specialization
    protected boolean isNil(MalNil nil) {
        return true;
    }

    @Fallback
    protected boolean isNil(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsTrueBuiltin extends BuiltinNode {
    protected IsTrueBuiltin() { super("true?"); }

    @Specialization
    protected boolean isTrue(boolean b) {
        return b == true;
    }

    @Fallback
    protected boolean isTrue(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsFalseBuiltin extends BuiltinNode {
    protected IsFalseBuiltin() { super("false?"); }

    @Specialization
    protected boolean isFalse(boolean b) {
        return b == false;
    }

    @Fallback
    protected boolean isFalse(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsSymbolBuiltin extends BuiltinNode {
    protected IsSymbolBuiltin() { super("symbol?"); }

    @Specialization
    protected boolean isSymbol(MalSymbol sym) {
        return true;
    }

    @Fallback
    protected boolean isSymbol(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsKeywordBuiltin extends BuiltinNode {

    protected IsKeywordBuiltin() { super("keyword?"); }

    @Specialization
    protected boolean isKeyword(MalKeyword kw) {
        return true;
    }

    @Fallback
    protected boolean isKeyword(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsVectorBuiltin extends BuiltinNode {

    protected IsVectorBuiltin() { super("vector?"); }

    @Specialization
    protected boolean isVector(MalVector vec) {
        return true;
    }

    @Fallback
    protected boolean isVector(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsSequentialBuiltin extends BuiltinNode {

    protected IsSequentialBuiltin() { super("sequential?"); }

    @Specialization
    protected Object isSequential(MalList list) {
        return true;
    }
    @Specialization
    protected Object isSequential(MalVector vec) {
        return true;
    }
    @Fallback
    protected Object isSequential(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsMapBuiltin extends BuiltinNode {

    protected IsMapBuiltin() { super("map?"); }

    @Specialization
    protected boolean isMap(MalMap map) {
        return true;
    }
    @Fallback
    protected boolean isMap(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsStringBuiltin extends BuiltinNode {

    protected IsStringBuiltin() { super("string?"); }

    @Specialization
    protected boolean isString(String val) {
        return true;
    }

    @Fallback
    protected boolean isString(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsNumberBuiltin extends BuiltinNode {

    protected IsNumberBuiltin() { super("number?"); }

    @Specialization
    protected boolean isNumber(long n) {
        return true;
    }

    @Fallback
    protected boolean isNumber(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsFnBuiltin extends BuiltinNode {

    protected IsFnBuiltin() { super("fn?"); }

    @Specialization
    protected boolean isFn(MalFunction fn) {
        return !fn.isMacro;
    }

    @Fallback
    protected boolean isFn(Object obj) {
        return false;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class IsMacroBuiltin extends BuiltinNode {

    protected IsMacroBuiltin() { super("macro?"); }

    @Specialization
    protected boolean isMacro(MalFunction fn) {
        return fn.isMacro;
    }

    @Fallback
    protected boolean isMacro(Object obj) {
        return false;
    }
}

/*************** Other ********************/

@NodeChild(value="ast", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class EvalBuiltin extends BuiltinNode {

    protected EvalBuiltin() { super("eval"); }

    @Specialization
    @TruffleBoundary
    protected Object eval(Object ast) {
        return language.evalForm(ast).call();
    }
}

@NodeChild(value="obj", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class ThrowBuiltin extends BuiltinNode {

    protected ThrowBuiltin() { super("throw"); }

    @TruffleBoundary
    @Specialization
    protected Object throwException(String obj) {
        throw new MalException(obj);
    }

    @TruffleBoundary
    @Fallback
    protected Object throwException(Object obj) {
        throw new MalException(obj);
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class SymbolBuiltin extends BuiltinNode {

    protected SymbolBuiltin() { super("symbol"); }

    @Specialization
    protected MalSymbol symbol(String str) {
        return MalSymbol.get(str);
    }

    @Specialization
    protected MalSymbol symbol(MalSymbol sym) {
        return sym;
    }
}

@GenerateNodeFactory
@NodeChild(value="arg", type=ReadArgNode.class)
abstract class KeywordBuiltin extends BuiltinNode {

    protected KeywordBuiltin() { super("keyword"); }

    @Specialization
    protected MalKeyword keyword(String arg) {
        return MalKeyword.get(arg);
    }

    @Specialization
    protected MalKeyword keyword(MalKeyword kw) {
        return kw;
    }
}

@NodeChild(value="prompt", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class ReadlineBuiltin extends BuiltinNode {

    protected ReadlineBuiltin() { super("readline"); }

    @Specialization
    @TruffleBoundary
    protected Object readline(String prompt) {
        language.out().print(prompt);
        language.out().flush();
        try {
            String s = language.in().readLine();
            return s == null ? MalNil.NIL : s;
        } catch (IOException ex) {
            throw new MalException(ex.getMessage());
        }
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class MetaBuiltin extends BuiltinNode {

    protected MetaBuiltin() { super("meta"); }

    @Specialization
    protected <T> Object meta(MetaHolder<T> arg) {
        return arg.getMeta();
    }

    @Fallback
    protected Object meta(Object obj) {
        return MalNil.NIL;
    }
}

@NodeChild(value="arg", type=ReadArgNode.class)
@NodeChild(value="meta", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class WithMetaBuiltin extends BuiltinNode {

    protected WithMetaBuiltin() { super("with-meta"); }

    @Specialization
    protected <T> Object withMeta(MetaHolder<T> holder, Object meta) {
        return holder.withMeta(meta);
    }
}

@GenerateNodeFactory
abstract class TimeMsBuiltin extends BuiltinNode {

    protected TimeMsBuiltin() { super("time-ms"); }

    @TruffleBoundary
    @Specialization
    protected long timeMs() {
        return System.nanoTime() / 1000000;
    }
}