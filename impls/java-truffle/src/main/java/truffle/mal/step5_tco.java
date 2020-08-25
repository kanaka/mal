package truffle.mal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.function.Function;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Scope;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.ControlFlowException;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.Source;

public class step5_tco {
    static final String LANGUAGE_ID = "mal_step5";

    public static void main(String[] args) throws IOException {
        boolean done = false;
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        var context = Context.create(LANGUAGE_ID);
        context.eval(LANGUAGE_ID, "(def! not (fn* [a] (if a false true)))");
        while (!done) {
            System.out.print("user> ");
            String s = reader.readLine();
            if (s == null) {
                done = true;
            } else {
                try {
                    Value val = context.eval(LANGUAGE_ID, s);
                    context.getBindings(LANGUAGE_ID).putMember("*1", val);
                    context.eval(LANGUAGE_ID, "(prn *1)");
                } catch (PolyglotException ex) {
                    if (ex.isGuestException()) {
                        System.out.println("Error: "+ex.getMessage());
                    } else {
                        throw ex;
                    }
                }
            }
        }
    }

    static class BuiltinFn implements TruffleObject {
        final Function<Object[], Object> fn;
        BuiltinFn(Function<Object[], Object> fn) {
            this.fn = fn;
        }
    }

    static abstract class MalNode extends Node {
        final Object form;
        protected MalNode(Object form) {
            this.form = form;
        }

        public abstract Object executeGeneric(VirtualFrame frame, MalEnv env);

        public long executeLong(VirtualFrame frame, MalEnv env) throws UnexpectedResultException {
            var value = executeGeneric(frame, env);
            if (value instanceof Long) {
                return (long)value;
            }
            throw new UnexpectedResultException(value);
        }

        public boolean executeBoolean(VirtualFrame frame, MalEnv env) throws UnexpectedResultException {
            var value = executeGeneric(frame, env);
            if (value instanceof Boolean) {
                return (boolean)value;
            }
            throw new UnexpectedResultException(value);
        }
    }

    private static MalNode formToNode(MalLanguage language, Object form, boolean tailPosition) {
        if (form instanceof MalSymbol) {
            return new LookupNode((MalSymbol)form);
        } else if (form instanceof MalVector) {
            return new VectorNode(language, (MalVector)form);
        } else if (form instanceof MalMap) {
            return new MapNode(language, (MalMap)form);
        } else if (form instanceof MalList && !((MalList)form).isEmpty()) {
            var list = (MalList)form;
            var head = list.head;
            if (MalSymbol.DEF_BANG.equals(head)) {
                return new DefNode(language, list);
            } else if (MalSymbol.LET_STAR.equals(head)) {
                return new LetNode(language, list, tailPosition);
            } else if (MalSymbol.DO.equals(head)) {
                return new DoNode(language, list, tailPosition);
            } else if (MalSymbol.IF.equals(head)) {
                return new IfNode(language, list, tailPosition);
            } else if (MalSymbol.FN_STAR.equals(head)) {
                return new FnNode(language, list);
            } else {
                return new ApplyNode(language, list, tailPosition);
            }
        } else {
            return new LiteralNode(form);
        }
    }

    static class LiteralNode extends MalNode {
        LiteralNode(Object form) {
            super(form);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            return form;
        }
    }

    static class VectorNode extends MalNode {
        @Children private MalNode[] elementNodes;

        VectorNode(MalLanguage language, MalVector vector) {
            super(vector);
            this.elementNodes = new MalNode[vector.size()];
            for (int i=0; i < vector.size(); i++) {
                elementNodes[i] = formToNode(language, vector.get(i), false);
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var elements = new ArrayList<>(elementNodes.length);
            for (int i=0; i < elementNodes.length; i++) {
                elements.add(elementNodes[i].executeGeneric(frame, env));
            }
            return MalVector.EMPTY.concat(elements);
        }
    }

    static class MapNode extends MalNode {
        @Children private MalNode[] nodes;
        MapNode(MalLanguage language, MalMap map) {
            super(map);
            nodes = new MalNode[map.map.size()*2];
            int i=0;
            for (var entry : map.map) {
                nodes[i++] = formToNode(language, entry.getKey(), false);
                nodes[i++] = formToNode(language, entry.getValue(), false);
            }
        }
        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var result = MalMap.EMPTY;
            for (int i=0; i < nodes.length; i += 2) {
                result = result.assoc(nodes[i].executeGeneric(frame, env), nodes[i+1].executeGeneric(frame, env));
            }
            return result;
        }
    }

    static class LookupNode extends MalNode {
        private final MalSymbol symbol;

        LookupNode(MalSymbol symbol) {
            super(symbol);
            this.symbol = symbol;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var result = env.get(symbol);
            if (result == null) {
                throw new MalException(symbol+" not found");
            }
            return result;
        }
    }

    @SuppressWarnings("serial")
    static class TailCallException extends ControlFlowException {
        final CallTarget callTarget;
        final Object[] args;
        TailCallException(CallTarget target, Object[] args) {
            this.callTarget = target;
            this.args = args;
        }
    }

    static class InvokeNode extends AbstractInvokeNode {
        final boolean tailPosition;
        @Child private IndirectCallNode callNode = Truffle.getRuntime().createIndirectCallNode();

        InvokeNode(boolean tailPosition) {
            this.tailPosition = tailPosition;
        }

        Object invoke(CallTarget target, Object[] args) {
            if (tailPosition) {
                throw new TailCallException(target, args);
            } else {
                while (true) {
                    try {
                        return callNode.call(target, args);
                    } catch (TailCallException ex) {
                        target = ex.callTarget;
                        args = ex.args;
                    }
                }
            }
        }
    }

    static class ApplyNode extends MalNode {
        @Child private MalNode fnNode;
        @Children private MalNode[] argNodes;
        @Child private InvokeNode invokeNode;

        ApplyNode(MalLanguage language, MalList list, boolean tailPosition) {
            super(list);
            fnNode = formToNode(language, list.head, false);
            argNodes = new MalNode[list.length-1];
            int i=0;
            list = list.tail;
            while (!list.isEmpty()) {
                argNodes[i++] = formToNode(language, list.head, false);
                list = list.tail;
            }
            invokeNode = new InvokeNode(tailPosition);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var fn = (MalFunction)fnNode.executeGeneric(frame, env);
            var args = new Object[argNodes.length+1];
            args[0] = fn.closedOverEnv;
            for (int i=0; i < argNodes.length; i++) {
                args[i+1] = argNodes[i].executeGeneric(frame, env);
            }
            return invokeNode.invoke(fn.callTarget, args);
        }
    }

    static class DefNode extends MalNode {
        private final MalSymbol symbol;
        @Child private MalNode valueNode;

        DefNode(MalLanguage language, MalList list) {
            super(list);
            this.symbol = (MalSymbol)list.tail.head;
            this.valueNode = formToNode(language, list.tail.tail.head, false);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var value = valueNode.executeGeneric(frame, env);
            env.set(symbol, value);
            return value;
        }
    }

    static class LetBindingNode extends Node {
        private final MalSymbol symbol;
        @Child private MalNode valueNode;

        LetBindingNode(MalLanguage language, MalSymbol symbol, Object valueForm) {
            this.symbol = symbol;
            this.valueNode = formToNode(language, valueForm, false);
        }

        public void executeGeneric(VirtualFrame frame, MalEnv env) {
            env.set(symbol, valueNode.executeGeneric(frame, env));
        }
    }

    static class LetNode extends MalNode {
        @Children private LetBindingNode[] bindings;
        @Child private MalNode bodyNode;

        LetNode(MalLanguage language, MalList form, boolean tailPosition) {
            super(form);
            var bindingForms = new ArrayList<Object>();
            assert form.tail.head instanceof Iterable<?>;
            ((Iterable<?>)form.tail.head).forEach(bindingForms::add);
            bindings = new LetBindingNode[bindingForms.size()/2];
            for (int i=0; i < bindingForms.size(); i+=2) {
                bindings[i/2] = new LetBindingNode(language, (MalSymbol)bindingForms.get(i), bindingForms.get(i+1));
            }
            bodyNode = formToNode(language, form.tail.tail.head, tailPosition);
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv outerEnv) {
            var innerEnv = new MalEnv(outerEnv);
            for (int i=0; i < bindings.length; i++) {
                bindings[i].executeGeneric(frame, innerEnv);
            }
            return bodyNode.executeGeneric(frame, innerEnv);
        }
    }

    /**
     * Represents a top-level evaluated form.
     */
    static class MalRootNode extends RootNode {
        final Object form;
        @Child MalNode body;

        MalRootNode(MalLanguage language, Object form) {
            super(language, new FrameDescriptor());
            this.form = form;
            // There's no stack to unwind at the top level, so
            // a top-level form is never in tail position.
            this.body = formToNode(language, form, false);
        }

        @Override
        public Object execute(VirtualFrame frame) {
            var ctx = lookupContextReference(MalLanguage.class).get();
            return body.executeGeneric(frame, ctx.globalEnv);
        }

        @Override
        public String toString() {
            return Printer.prStr(form, true);
        }
    }

    static class DoNode extends MalNode {
        @Children private MalNode[] bodyNodes;

        DoNode(MalLanguage language, MalList form, boolean tailPosition) {
            super(form);
            bodyNodes = new MalNode[form.length-1];
            int i = 0;
            for (var f : form.tail) {
                bodyNodes[i++] = formToNode(language, f, tailPosition && i == form.length-2);
            }
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            if (bodyNodes.length == 0) {
                return MalNil.NIL;
            }

            for (int i=0; i < bodyNodes.length-1; i++) {
                bodyNodes[i].executeGeneric(frame, env);
            }
            return bodyNodes[bodyNodes.length-1].executeGeneric(frame, env);
        }
    }

    static class IfNode extends MalNode {
        @Child private MalNode conditionNode;
        @Child private MalNode trueNode;
        @Child private MalNode falseNode;

        IfNode(MalLanguage language, MalList form, boolean tailPosition) {
            super(form);
            conditionNode = formToNode(language, form.tail.head, false);
            trueNode = formToNode(language, form.tail.tail.head, tailPosition);
            var falseForm = form.tail.tail.tail.head;
            falseNode = falseForm == null ? null : formToNode(language, falseForm, tailPosition);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var val = conditionNode.executeGeneric(frame, env);
            if (val == MalNil.NIL || Boolean.FALSE.equals(val)) {
                if (falseNode == null) {
                    return MalNil.NIL;
                } else {
                    return falseNode.executeGeneric(frame, env);
                }
            } else {
                return trueNode.executeGeneric(frame, env);
            }
        }
    }

    static abstract class AbstractBindArgNode extends Node {
        protected final MalSymbol symbol;
        protected final int argPos;

        protected AbstractBindArgNode(MalSymbol symbol, int argPos) {
            this.symbol = symbol;
            this.argPos = argPos;
        }

        public abstract void execute(VirtualFrame frame, MalEnv env);
    }

    static class BindArgNode extends AbstractBindArgNode {

        public BindArgNode(MalSymbol symbol, int argPos) {
            super(symbol, argPos);
        }

        @Override
        public void execute(VirtualFrame frame, MalEnv env) {
            env.set(symbol, frame.getArguments()[argPos]);
        }
    }

    static class BindVarargsNode extends BindArgNode {
        public BindVarargsNode(MalSymbol symbol, int argPos) {
            super(symbol, argPos);
        }

        @TruffleBoundary
        private MalList buildVarArgsList(Object[] args) {
            MalList varArgs = MalList.EMPTY;
            for (int i=args.length-1; i >= argPos; --i) {
                varArgs = varArgs.cons(args[i]);
            }
            return varArgs;
        }

        @Override
        public void execute(VirtualFrame frame, MalEnv env) {
            env.set(symbol, buildVarArgsList(frame.getArguments()));
        }
    }
    /**
     * Root node of a user-defined function, responsible for managing
     * the environment when the function is invoked.
     */
    static class FnRootNode extends RootNode {
        final MalList form;
        final int numArgs;
        @Children AbstractBindArgNode[] bindNodes;
        @Child MalNode bodyNode;

        FnRootNode(MalLanguage language, MalList form) {
            super(language, new FrameDescriptor());
            this.form = form;
            var argNamesList = new ArrayList<MalSymbol>();
            assert form.tail.head instanceof Iterable<?>;
            var foundAmpersand = false;
            for (var name : (Iterable<?>)form.tail.head) {
                if (MalSymbol.AMPERSAND.equals(name)) {
                    foundAmpersand = true;
                } else {
                    argNamesList.add((MalSymbol)name);
                }
            }
            this.numArgs = foundAmpersand? -1 : argNamesList.size();
            this.bindNodes = new AbstractBindArgNode[argNamesList.size()];
            for (int i=0; i < argNamesList.size(); i++) {
                if (numArgs == -1 && i == argNamesList.size()-1) {
                    bindNodes[i] = new BindVarargsNode(argNamesList.get(i), i+1);
                } else {
                    bindNodes[i] = new BindArgNode(argNamesList.get(i), i+1);
                }
            }
            this.bodyNode = formToNode(language, form.tail.tail.head, true);
        }

        @ExplodeLoop
        @Override
        public Object execute(VirtualFrame frame) {
            var env = new MalEnv((MalEnv)frame.getArguments()[0]);
            for (int i=0; i < bindNodes.length; i++) {
                bindNodes[i].execute(frame, env);
            }
            return bodyNode.executeGeneric(frame, env);
        }
    }

    /**
     * Node representing a (fn* ...) form.
     */
    static class FnNode extends MalNode {
        final FnRootNode fnRoot;
        final RootCallTarget fnCallTarget;

        FnNode(MalLanguage language, MalList form) {
            super(form);
            fnRoot = new FnRootNode(language, form);
            this.fnCallTarget = Truffle.getRuntime().createCallTarget(fnRoot);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            return new MalFunction(fnCallTarget, env, fnRoot.numArgs);
        }
    }

    final static class MalContext {
        final MalEnv globalEnv;
        final Iterable<Scope> topScopes;
        final PrintStream out;
        final BufferedReader in;

        MalContext(MalLanguage language) {
            globalEnv = Core.newGlobalEnv(MalLanguage.class, language);
            topScopes = Collections.singleton(Scope.newBuilder("global", globalEnv).build());
            out = System.out;
            in = new BufferedReader(new InputStreamReader(System.in));
        }
    }

    @TruffleLanguage.Registration(
            id=LANGUAGE_ID,
            name=LANGUAGE_ID,
            defaultMimeType = "application/x-"+LANGUAGE_ID,
            characterMimeTypes = "application/x-"+LANGUAGE_ID)
    public final static class MalLanguage extends TruffleLanguage<MalContext> implements IMalLanguage {
        @Override
        protected MalContext createContext(Env env) {
            return new MalContext(this);
        }

        @Override
        public CallTarget evalForm(Object form) {
            var root = new MalRootNode(this, form);
            return Truffle.getRuntime().createCallTarget(root);
        }

        @Override
        public AbstractInvokeNode invokeNode() {
            return new InvokeNode(false);
        }

        @Override
        protected CallTarget parse(ParsingRequest request) throws Exception {
            Source source = request.getSource();
            String s = source.getCharacters().toString();
            return evalForm(Reader.readStr(s));
        }

        @Override
        protected Iterable<Scope> findTopScopes(MalContext context) {
            return context.topScopes;
        }

        @Override
        public PrintStream out() {
            return getCurrentContext(MalLanguage.class).out;
        }

        @Override
        public BufferedReader in() {
            return getCurrentContext(MalLanguage.class).in;
        }
    }
}
