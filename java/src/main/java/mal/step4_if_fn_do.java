package mal;

import java.io.IOException;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import mal.types.*;
import mal.readline;
import mal.reader;
import mal.printer;
import mal.env.Env;
import mal.core;

public class step4_if_fn_do {
    // read
    public static MalVal READ(String str) throws MalThrowable {
        return reader.read_str(str);
    }

    // eval
    public static MalVal eval_ast(MalVal ast, Env env) throws MalThrowable {
        if (ast instanceof MalSymbol) {
            MalSymbol sym = (MalSymbol)ast;
            return env.get(sym.getName());
        } else if (ast instanceof MalList) {
            MalList old_lst = (MalList)ast;
            MalList new_lst = ast.list_Q() ? new MalList()
                                           : (MalList)new MalVector();
            for (MalVal mv : (List<MalVal>)old_lst.value) {
                new_lst.conj_BANG(EVAL(mv, env));
            }
            return new_lst;
        } else if (ast instanceof MalHashMap) {
            MalHashMap new_hm = new MalHashMap();
            Iterator it = ((MalHashMap)ast).value.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry entry = (Map.Entry)it.next();
                new_hm.value.put(entry.getKey(), EVAL((MalVal)entry.getValue(), env));
            }
            return new_hm;
        } else {
            return ast;
        }
    }

    public static MalVal EVAL(MalVal orig_ast, Env env) throws MalThrowable {
        MalVal a0, a1,a2, a3, res;
        MalList el;
        //System.out.println("EVAL: " + printer._pr_str(orig_ast, true));
        if (!orig_ast.list_Q()) {
            return eval_ast(orig_ast, env);
        }

        // apply list
        MalList ast = (MalList)orig_ast;
        if (ast.size() == 0) { return ast; }
        a0 = ast.nth(0);
        String a0sym = a0 instanceof MalSymbol ? ((MalSymbol)a0).getName()
                                               : "__<*fn*>__";
        switch (a0sym) {
        case "def!":
            a1 = ast.nth(1);
            a2 = ast.nth(2);
            res = EVAL(a2, env);
            env.set(((MalSymbol)a1).getName(), res);
            return res;
        case "let*":
            a1 = ast.nth(1);
            a2 = ast.nth(2);
            MalSymbol key;
            MalVal val;
            Env let_env = new Env(env);
            for(int i=0; i<((MalList)a1).size(); i+=2) {
                key = (MalSymbol)((MalList)a1).nth(i);
                val = ((MalList)a1).nth(i+1);
                let_env.set(key.getName(), EVAL(val, let_env));
            }
            return EVAL(a2, let_env);
        case "do":
            el = (MalList)eval_ast(ast.rest(), env);
            return el.nth(el.size()-1);
        case "if":
            a1 = ast.nth(1);
            MalVal cond = EVAL(a1, env);
            if (cond == types.Nil || cond == types.False) {
                // eval false slot form
                if (ast.size() > 3) {
                    a3 = ast.nth(3);
                    return EVAL(a3, env);
                } else {
                    return types.Nil;
                }
            } else {
                // eval true slot form
                a2 = ast.nth(2);
                return EVAL(a2, env);
            }
        case "fn*":
            final MalList a1f = (MalList)ast.nth(1);
            final MalVal a2f = ast.nth(2);
            final Env cur_env = env;
            return new MalFunction () {
                public MalVal apply(MalList args) throws MalThrowable {
                    return EVAL(a2f, new Env(cur_env, a1f, args));
                }
            };
        default:
            el = (MalList)eval_ast(ast, env);
            MalFunction f = (MalFunction)el.nth(0);
            return f.apply(el.rest());
        }
    }

    // print
    public static String PRINT(MalVal exp) {
        return printer._pr_str(exp, true);
    }

    // repl
    public static MalVal RE(Env env, String str) throws MalThrowable {
        return EVAL(READ(str), env);
    }

    public static void main(String[] args) throws MalThrowable {
        String prompt = "user> ";

        Env repl_env = new Env(null);

        // core.java: defined using Java
        for (String key : core.ns.keySet()) {
            repl_env.set(key, core.ns.get(key));
        }

        // core.mal: defined using the language itself
        RE(repl_env, "(def! not (fn* (a) (if a false true)))");
        
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
                continue;
            } catch (MalThrowable t) {
                System.out.println("Error: " + t.getMessage());
                continue;
            } catch (Throwable t) {
                System.out.println("Uncaught " + t + ": " + t.getMessage());
                continue;
            }
        }
    }
}
