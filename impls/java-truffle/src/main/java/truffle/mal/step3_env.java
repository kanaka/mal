package truffle.mal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.function.Function;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.Source;

public class step3_env {
    static final String LANGUAGE_ID = "mal_step3";

    public static void main(String[] args) throws IOException {
        boolean done = false;
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        var context = Context.create(LANGUAGE_ID);
        while (!done) {
            System.out.print("user> ");
            String s = reader.readLine();
            if (s == null) {
                done = true;
            } else {
                try {
                    Value val = context.eval(LANGUAGE_ID, s);
                    System.out.println(val.toString());
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

    private static MalNode formToNode(Object form) {
        if (form instanceof MalSymbol) {
            return new LookupNode((MalSymbol)form);
        } else if (form instanceof MalVector) {
            return new VectorNode((MalVector)form);
        } else if (form instanceof MalMap) {
            return new MapNode((MalMap)form);
        } else if (form instanceof MalList && !((MalList)form).isEmpty()) {
            var list = (MalList)form;
            var head = list.head;
            if (MalSymbol.DEF_BANG.equals(head)) {
                return new DefNode(list);
            } else if (MalSymbol.LET_STAR.equals(head)) {
                return new LetNode(list);
            } else {
                return new ApplyNode(list);
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

        VectorNode(MalVector vector) {
            super(vector);
            this.elementNodes = new MalNode[vector.size()];
            for (int i=0; i < vector.size(); i++) {
                elementNodes[i] = formToNode(vector.get(i));
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
        MapNode(MalMap map) {
            super(map);
            nodes = new MalNode[map.map.size()*2];
            int i=0;
            for (var entry : map.map) {
                nodes[i++] = formToNode(entry.getKey());
                nodes[i++] = formToNode(entry.getValue());
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

    static class ApplyNode extends MalNode {
        @Child private MalNode fnNode;
        @Children private MalNode[] argNodes;

        ApplyNode(MalList list) {
            super(list);
            fnNode = formToNode(list.head);
            argNodes = new MalNode[list.length-1];
            int i=0;
            list = list.tail;
            while (!list.isEmpty()) {
                argNodes[i++] = formToNode(list.head);
                list = list.tail;
            }
        }

        @Override
        public Object executeGeneric(VirtualFrame frame, MalEnv env) {
            var fn = (BuiltinFn)fnNode.executeGeneric(frame, env);
            var args = new Object[argNodes.length];
            for (int i=0; i < args.length; i++) {
                args[i] = argNodes[i].executeGeneric(frame, env);
            }
            return fn.fn.apply(args);
        }
    }

    static class DefNode extends MalNode {
        private final MalSymbol symbol;
        @Child private MalNode valueNode;

        DefNode(MalList list) {
            super(list);
            this.symbol = (MalSymbol)list.tail.head;
            this.valueNode = formToNode(list.tail.tail.head);
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
        LetBindingNode(MalSymbol symbol, Object valueForm) {
            this.symbol = symbol;
            this.valueNode = formToNode(valueForm);
        }

        public void executeGeneric(VirtualFrame frame, MalEnv env) {
            env.set(symbol, valueNode.executeGeneric(frame, env));
        }
    }

    static class LetNode extends MalNode {
        @Children private LetBindingNode[] bindings;
        @Child private MalNode bodyNode;

        LetNode(MalList form) {
            super(form);
            var bindingForms = new ArrayList<Object>();
            assert form.tail.head instanceof Iterable<?>;
            ((Iterable<?>)form.tail.head).forEach(bindingForms::add);
            bindings = new LetBindingNode[bindingForms.size()/2];
            for (int i=0; i < bindingForms.size(); i+=2) {
                bindings[i/2] = new LetBindingNode((MalSymbol)bindingForms.get(i), bindingForms.get(i+1));
            }
            bodyNode = formToNode(form.tail.tail.head);
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

    static class MalRootNode extends RootNode {
        final Object form;
        @Child MalNode body;

        MalRootNode(TruffleLanguage<?> language, Object form) {
            super(language, new FrameDescriptor());
            this.form = form;
            this.body = formToNode(form);
        }

        @Override
        public Object execute(VirtualFrame frame) {
            var ctx = lookupContextReference(MalLanguage.class).get();
            return body.executeGeneric(frame, ctx.globalEnv);
        }
    }

    final static class MalContext {
        final MalEnv globalEnv = new MalEnv(MalLanguage.class);
    }

    @TruffleLanguage.Registration(
            id=LANGUAGE_ID,
            name=LANGUAGE_ID,
            defaultMimeType = "application/x-"+LANGUAGE_ID,
            characterMimeTypes = "application/x-"+LANGUAGE_ID)
    public final static class MalLanguage extends TruffleLanguage<MalContext> {
        @Override
        protected MalContext createContext(Env env) {
            var ctx = new MalContext();
            ctx.globalEnv.set(MalSymbol.get("+"), new BuiltinFn(args -> { return (long)args[0]+(long)args[1]; }));
            ctx.globalEnv.set(MalSymbol.get("-"), new BuiltinFn(args -> { return (long)args[0]-(long)args[1]; }));
            ctx.globalEnv.set(MalSymbol.get("*"), new BuiltinFn(args -> { return (long)args[0]*(long)args[1]; }));
            ctx.globalEnv.set(MalSymbol.get("/"), new BuiltinFn(args -> { return (long)args[0]/(long)args[1]; }));
            return ctx;
        }

        @Override
        protected CallTarget parse(ParsingRequest request) throws Exception {
            Source source = request.getSource();
            String s = source.getCharacters().toString();
            var root = new MalRootNode(this, Reader.readStr(s));
            return Truffle.getRuntime().createCallTarget(root);
        }
    }
}
