package mal;

import java.io.IOException;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import mal.types.*;
import mal.readline;
import mal.reader;
import mal.printer;

public class step2_eval {
    // read
    public static MalVal READ(String str) throws MalThrowable {
        return reader.read_str(str);
    }

    // eval
    public static MalVal EVAL(MalVal orig_ast, Map<String, MalVal> env) throws MalThrowable {
        // System.out.println("EVAL: " + printer._pr_str(orig_ast, true));

        if (orig_ast instanceof MalSymbol) {
            final String key = ((MalSymbol)orig_ast).getName();
            final MalVal val = env.get(key);
            if (val == null)
                throw new MalException("'" + key + "' not found");
            return val;
        } else if (orig_ast instanceof MalVector) {
            final MalList old_lst = (MalList)orig_ast;
            final MalVector new_lst = new MalVector();
            for (MalVal mv : (List<MalVal>)old_lst.value) {
                new_lst.conj_BANG(EVAL(mv, env));
            }
            return new_lst;
        } else if (orig_ast instanceof MalHashMap) {
            final Map<String, MalVal> old_hm = ((MalHashMap)orig_ast).value;
            MalHashMap new_hm = new MalHashMap();
            for (Map.Entry<String, MalVal> entry : old_hm.entrySet()) {
                new_hm.value.put(entry.getKey(), EVAL((MalVal)entry.getValue(), env));
            }
            return new_hm;
        } else if (!orig_ast.list_Q()) {
            return orig_ast;
        }
        final MalList ast = (MalList)orig_ast;
        // apply list
        if (ast.size() == 0) { return ast; }
        final MalVal f = EVAL(ast.nth(0), env);
        if (!(f instanceof ILambda))
            throw new MalError("cannot apply " + printer._pr_str(ast, true));
        final MalList args = new MalList();
        for (int i=1; i<ast.size(); i++)
            args.conj_BANG(EVAL(ast.nth(i), env));
        return ((ILambda)f).apply(args);
    }

    // print
    public static String PRINT(MalVal exp) {
        return printer._pr_str(exp, true);
    }

    // repl
    public static MalVal RE(Map<String, MalVal> env, String str) throws MalThrowable {
        return EVAL(READ(str), env);
    }

    static MalFunction add = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).add((MalInteger)a.nth(1));
        }
    };
    static MalFunction subtract = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).subtract((MalInteger)a.nth(1));
        }
    };
    static MalFunction multiply = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).multiply((MalInteger)a.nth(1));
        }
    };
    static MalFunction divide = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).divide((MalInteger)a.nth(1));
        }
    };


    public static void main(String[] args) throws MalThrowable {
        String prompt = "user> ";

        Map<String, MalVal> repl_env = new HashMap<String, MalVal>();
        repl_env.put("+", add);
        repl_env.put("-", subtract);
        repl_env.put("*", multiply);
        repl_env.put("/", divide);

        if (args.length > 0 && args[0].equals("--raw")) {
            readline.mode = readline.Mode.JAVA;
        }
        while (true) {
            String line;
            try {
                line = readline.readline(prompt);
                if (line == null) { continue; }
            } catch (readline.EOFException e) {
                break;
            } catch (IOException e) {
                System.out.println("IOException: " + e.getMessage());
                break;
            }
            try {
                System.out.println(PRINT(RE(repl_env, line)));
            } catch (MalContinue e) {
            } catch (MalException e) {
                System.out.println("Error: " + printer._pr_str(e.getValue(), false));
            } catch (MalThrowable t) {
                System.out.println("Error: " + t.getMessage());
            } catch (Throwable t) {
                System.out.println("Uncaught " + t + ": " + t.getMessage());
            }
        }
    }
}
