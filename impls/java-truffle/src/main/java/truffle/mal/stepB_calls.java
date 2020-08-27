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
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Scope;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.ControlFlowException;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.Source;

public class stepB_calls {
    static final String LANGUAGE_ID = "mal_stepB";

    public static void main(String[] args) throws IOException {
        boolean done = false;
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        var context = Context.create(LANGUAGE_ID);
        context.eval(LANGUAGE_ID, "(def! not (fn* [a] (if a false true)))");
        context.eval(LANGUAGE_ID, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");
        context.eval(LANGUAGE_ID, "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
        context.eval(LANGUAGE_ID, "(def! *host-language* \"java-truffle\")");

        var buf = new StringBuilder();
        buf.append("(def! *ARGV* (list");
        for (int i=1; i < args.length; i++) {
            buf.append(' ');
            buf.append(Printer.prStr(args[i], true));
        }
        buf.append("))");
        context.eval(LANGUAGE_ID, buf.toString());

        if (args.length > 0) {
            context.eval(LANGUAGE_ID, "(load-file \""+args[0]+"\")");
            return;
        }

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

    private static boolean isPair(Object obj) {
        return (obj instanceof MalList && ((MalList)obj).length > 0)
               ||
               (obj instanceof MalVector && ((MalVector)obj).size() > 0);
    }

    private static Object quasiquote(Object form) {
        if (!isPair(form)) {
            return MalList.EMPTY.cons(form).cons(MalSymbol.QUOTE);
        }
        MalList list = (form instanceof MalVector) ? ((MalVector)form).toList() : (MalList)form;
        if (MalSymbol.UNQUOTE.equals(list.head)) {
            return list.tail.head;
        }
        var result = new ArrayList<Object>();
        if (isPair(list.head) && MalSymbol.SPLICE_UNQUOTE.equals(((MalList)list.head).head)) {
            result.add(MalSymbol.get("concat"));
            result.add(((MalList)list.head).tail.head);
        } else {
            result.add(MalSymbol.get("cons"));
            result.add(quasiquote(list.head));
        }
        result.add(quasiquote(list.tail));
        return MalList.from(result);
    }

    @TruffleBoundary
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
            if (MalSymbol.DEF_BANG.equals(head) || MalSymbol.DEFMACRO.equals(head)) {
                return new DefNode(language, list);
            } else if (MalSymbol.LET_STAR.equals(head)) {
                return new LetNode(language, list, tailPosition);
            } else if (MalSymbol.DO.equals(head)) {
                return new DoNode(language, list, tailPosition);
            } else if (MalSymbol.IF.equals(head)) {
                return new IfNode(language, list, tailPosition);
            } else if (MalSymbol.FN_STAR.equals(head)) {
                return new FnNode(language, list);
            } else if (MalSymbol.QUOTE.equals(head)) {
                return new QuoteNode(language, list);
            } else if (MalSymbol.QUASIQUOTE.equals(head)) {
                return formToNode(language, quasiquote(list.tail.head), tailPosition);
            } else if (MalSymbol.MACROEXPAND.equals(head)) {
                return new MacroexpandNode(list);
            } else if (MalSymbol.TRY.equals(head)) {
                return new TryNode(language, list, tailPosition);
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
                throw new MalException("'"+symbol+"' not found");
            }
            return result;
        }
    }

    @SuppressWarnings("serial")
    static class TailCallException extends ControlFlowException {
        final CallTarget callTarget;
        final Object[] args;
        @TruffleBoundary
        TailCallException(CallTarget target, Object[] args) {
            this.callTarget = target;
            this.args = args;
        }
    }

    static class InvokeNode extends AbstractInvokeNode {
        final boolean tailPosition;
        @CompilationFinal private boolean initialized = false;
        @CompilationFinal private boolean usingCachedTarget;
        @CompilationFinal private CallTarget cachedTarget;
        @CompilationFinal @Child private DirectCallNode directCallNode;
        @CompilationFinal @Child private IndirectCallNode indirectCallNode;

        InvokeNode(boolean tailPosition) {
            this.tailPosition = tailPosition;
        }

        Object invoke(CallTarget target, Object[] args) {
            return invoke(target, args, true);
        }

        Object invoke(CallTarget target, Object[] args, boolean allowTailCall) {
            if (tailPosition && allowTailCall) {
                throw new TailCallException(target, args);
            } else {
                if (!initialized) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    initialized = true;
                    usingCachedTarget = true;
                    cachedTarget = target;
                    directCallNode = Truffle.getRuntime().createDirectCallNode(target);
                }
                while (true) {
                    try {
                        if (usingCachedTarget) {
                            if (cachedTarget == target) {
                                return directCallNode.call(args);
                            }
                            CompilerDirectives.transferToInterpreterAndInvalidate();
                            usingCachedTarget = false;
                            indirectCallNode = Truffle.getRuntime().createIndirectCallNode();
                        }
                        return indirectCallNode.call(target, args);
                    } catch (TailCallException ex) {
                        target = ex.callTarget;
                        args = ex.args;
                    }
                }
            }
        }
    }

    private static MalFunction getMacroFn(MalEnv env, Object form) {
        if (!(form instanceof MalList))
            return null;
        MalList list = (MalList)form;
        if (!(list.head instanceof MalSymbol))
            return null;
        MalSymbol fnSym = (MalSymbol)list.head;
        var obj = env.get(fnSym);
        if (obj == null)
            return null;
        if (!(obj instanceof MalFunction))
            return null;
        MalFunction fn = (MalFunction)obj;
        return fn.isMacro ? fn : null;
    }

    static Object macroexpand(InvokeNode invokeNode, MalEnv env, Object form) {
        var fn = getMacroFn(env, form);
        while (fn != null) {
            MalList list = (MalList)form;
            var args = new Object[(int)list.length];
            args[0] = fn.closedOverEnv;
            int i=1;
            list = list.tail;
            while (!list.isEmpty()) {
                args[i++] = list.head;
                list = list.tail;
            }
            form = invokeNode.invoke(fn.callTarget, args, false);
            fn = getMacroFn(env, form);
        }
        return form;
    }

    static class MacroexpandNode extends MalNode {
        @Child private InvokeNode invokeNode = new InvokeNode(false);
        private final Object body;

        MacroexpandNode(MalList form) {
            super(form);
            this.body = form.tail.head;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            return macroexpand(invokeNode, env, body);
        }
    }

    static class ApplyNode extends MalNode {
        final MalLanguage language;
        @Child private MalNode fnNode;
        @Children private MalNode[] argNodes;
        @Child private InvokeNode invokeNode;
        @CompilationFinal private boolean initialized = false;
        @CompilationFinal private boolean usingCachedFn;
        @CompilationFinal private MalFunction cachedFn;

        ApplyNode(MalLanguage language, MalList list, boolean tailPosition) {
            super(list);
            this.language = language;
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

        @TruffleBoundary
        private CallTarget applyMacro(MalEnv env, MalFunction fn) {
            Object[] args = new Object[argNodes.length+1];
            args[0] = fn.closedOverEnv;
            for (int i=0; i < argNodes.length; ++i) {
                args[i+1] = argNodes[i].form;
            }
            // We should never throw a tail call during expansion!
            Object form = invokeNode.invoke(fn.callTarget, args, false);
            var result = macroexpand(invokeNode, env, form);
            var newRoot = new MalRootNode(language, result, env, invokeNode.tailPosition);
            return Truffle.getRuntime().createCallTarget(newRoot);
        }

        @ExplodeLoop
        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var fn = (MalFunction)fnNode.executeGeneric(frame, env);
            if (!initialized) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                initialized = true;
                cachedFn = fn;
                usingCachedFn = true;
            }
            if (usingCachedFn) {
                if (fn != cachedFn) {
                    CompilerDirectives.transferToInterpreterAndInvalidate();
                    usingCachedFn = false;
                } else {
                    fn = cachedFn;
                }
            }
            if (fn.isMacro) {
                // Mal's macro semantics are... interesting. To preserve them in the
                // general case, we must re-expand a macro each time it's applied.
                // Executing the result means turning it into a Truffle AST, creating
                // a CallTarget, calling it, and then throwing it away.
                // This is TERRIBLE for performance! Truffle should not be used like this!
                var target = applyMacro(env, fn);
                return invokeNode.invoke(target, new Object[] {}, false);
            } else {
                var args = new Object[argNodes.length+1];
                args[0] = fn.closedOverEnv;
                for (int i=0; i < argNodes.length; i++) {
                    args[i+1] = argNodes[i].executeGeneric(frame, env);
                }
                return invokeNode.invoke(fn.callTarget, args, fn.canBeTailCalled);
            }
        }
    }

    static class DefNode extends MalNode {
        private final MalSymbol symbol;
        private final boolean macro;
        @Child private MalNode valueNode;

        DefNode(MalLanguage language, MalList list) {
            super(list);
            this.symbol = (MalSymbol)list.tail.head;
            this.macro = MalSymbol.DEFMACRO.equals(list.head);
            this.valueNode = formToNode(language, list.tail.tail.head, false);
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var value = valueNode.executeGeneric(frame, env);
            if (macro) {
                value = new MalFunction((MalFunction)value, true);
            }
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
     * Represents a form to be evaluated, together with an environment.
     */
    static class MalRootNode extends RootNode {
        final Object form;
        final MalEnv env;
        @Child MalNode body;

        MalRootNode(MalLanguage language, Object form, MalEnv env, boolean tailPosition) {
            super(language, new FrameDescriptor());
            this.form = form;
            // There's no stack to unwind at the top level, so
            // a top-level form is never in tail position.
            this.body = formToNode(language, form, tailPosition);
            this.env = env;
        }

        @Override
        public Object execute(VirtualFrame frame) {
            return body.executeGeneric(frame, env);
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
                bodyNodes[i++] = formToNode(language, f, tailPosition && i == form.length-1);
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
            if (val == MalNil.NIL || val == Boolean.FALSE) {
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

        @Override
        public String toString() {
            return form.toString();
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

    static class QuoteNode extends MalNode {
        final Object quoted;

        QuoteNode(MalLanguage language, MalList form) {
            super(form);
            quoted = form.tail.head;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            return quoted;
        }
    }

    static class TryNode extends MalNode {
        @Child private MalNode tryBody;
        @Child private MalNode catchBody;
        final MalSymbol exSymbol;

        TryNode(MalLanguage language, MalList form, boolean tailPosition) {
            super(form);
            var tryForm = form.tail.head;
            var catchForm = (MalList)form.tail.tail.head;
            // We don't allow tail calls inside a try body, because
            // they'd get thrown past the catch that should catch subsequent failures.
            this.tryBody = formToNode(language, tryForm, false);
            if (catchForm != null && MalSymbol.CATCH.equals(catchForm.head)) {
                exSymbol = (MalSymbol)catchForm.tail.head;
                catchBody = formToNode(language, catchForm.tail.tail.head, tailPosition);
            } else {
                exSymbol = null;
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            try {
                return tryBody.executeGeneric(frame, env);
            } catch (MalException ex) {
                if (catchBody == null) {
                    throw ex;
                }
                var catchEnv = new MalEnv(env);
                catchEnv.set(exSymbol, ex.obj);
                return catchBody.executeGeneric(frame, catchEnv);
            }
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
            var env = getCurrentContext(MalLanguage.class).globalEnv;
            var root = new MalRootNode(this, form, env, false);
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
