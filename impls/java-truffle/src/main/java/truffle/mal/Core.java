package truffle.mal;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
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
    }

    static MalEnv newGlobalEnv(Class<? extends TruffleLanguage<?>> languageClass, TruffleLanguage<?> language) {
        var env = new MalEnv(languageClass);
        for (var entry : NS.entrySet()) {
            var root = new BuiltinRootNode(language, entry.getValue());
            var fnVal = new MalFunction(Truffle.getRuntime().createCallTarget(root), null, root.getNumArgs());
            env.set(MalSymbol.get(entry.getKey()), fnVal);
        }
        return env;
    }
}

abstract class AbstractInvokeNode extends Node {
    abstract Object invoke(CallTarget target, Object[] args);
}
/** A hack to make the EvalBuiltin sharable across languages.
 */
interface IMalLanguage {
    CallTarget evalForm(Object form);
    AbstractInvokeNode invokeNode();
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
        // The correct thing is to use the output stream associated with our language context.
        // However, since each step is effectively its own language, and we wish
        // to share this node among them, we'll just cheat and call System.out directly.
        System.out.println(buf.toString());
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
        System.out.println(buf.toString());
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

/*************** Other ********************/

@NodeChild(value="ast", type=ReadArgNode.class)
@GenerateNodeFactory
abstract class EvalBuiltin extends BuiltinNode {

    protected EvalBuiltin() { super("eval"); }

    @Specialization
    protected Object eval(Object ast) {
        return language.evalForm(ast).call();
    }
}