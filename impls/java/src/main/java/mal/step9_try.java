package mal;

import java.io.IOException;

import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.List;
import java.util.Map;
import mal.types.*;
import mal.readline;
import mal.reader;
import mal.printer;
import mal.env.Env;
import mal.core;

public class step9_try {
    // read
    public static MalVal READ(String str) throws MalThrowable {
        return reader.read_str(str);
    }

    // eval
    public static Boolean starts_with(MalVal ast, String sym) {
        //  Liskov, forgive me
        if (ast instanceof MalList && !(ast instanceof MalVector) && ((MalList)ast).size() == 2) {
            MalVal a0 = ((MalList)ast).nth(0);
            return a0 instanceof MalSymbol && ((MalSymbol)a0).getName().equals(sym);
        }
        return false;
    }

    public static MalVal quasiquote(MalVal ast) {
        if ((ast instanceof MalSymbol || ast instanceof MalHashMap))
            return new MalList(new MalSymbol("quote"), ast);

        if (!(ast instanceof MalList))
            return ast;

        if (starts_with(ast, "unquote"))
            return ((MalList)ast).nth(1);

        MalVal res = new MalList();
        for (Integer i=((MalList)ast).size()-1; 0<=i; i--) {
            MalVal elt = ((MalList)ast).nth(i);
            if (starts_with(elt, "splice-unquote"))
                res = new MalList(new MalSymbol("concat"), ((MalList)elt).nth(1), res);
            else
                res = new MalList(new MalSymbol("cons"), quasiquote(elt), res);
        }
        if (ast instanceof MalVector)
            res = new MalList(new MalSymbol("vec"), res);
        return res;
    }

    public static MalVal EVAL(MalVal orig_ast, Env env) throws MalThrowable {
        while (true) {

        final MalVal dbgeval = env.get("DEBUG-EVAL");
        if (dbgeval != null && dbgeval != types.Nil && dbgeval != types.False)
            System.out.println("EVAL: " + printer._pr_str(orig_ast, true));

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
        MalVal a0, a1,a2, a3, res;
        // apply list
        if (ast.size() == 0) { return ast; }
        a0 = ast.nth(0);
        String a0sym = a0 instanceof MalSymbol ? ((MalSymbol)a0).getName()
                                               : "__<*fn*>__";
        switch (a0sym) {
        case "def!":
            a1 = ast.nth(1);
            a2 = ast.nth(2);
            res = EVAL(a2, env);
            env.set(((MalSymbol)a1), res);
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
                let_env.set(key, EVAL(val, let_env));
            }
            orig_ast = a2;
            env = let_env;
            break;
        case "quote":
            return ast.nth(1);
        case "quasiquote":
            orig_ast = quasiquote(ast.nth(1));
            break;
        case "defmacro!":
            a1 = ast.nth(1);
            a2 = ast.nth(2);
            res = EVAL(a2, env);
            res = res.copy();
            ((MalFunction)res).setMacro();
            env.set((MalSymbol)a1, res);
            return res;
        case "try*":
            try {
                return EVAL(ast.nth(1), env);
            } catch (Throwable t) {
                if (ast.size() > 2) {
                    MalVal exc;
                    a2 = ast.nth(2);
                    MalVal a20 = ((MalList)a2).nth(0);
                    if (((MalSymbol)a20).getName().equals("catch*")) {
                        if (t instanceof MalException) {
                            exc = ((MalException)t).getValue();
                        } else {
                            StringWriter sw = new StringWriter();
                            t.printStackTrace(new PrintWriter(sw));
                            String tstr = sw.toString();
                            exc = new MalString(t.getMessage() + ": " + tstr);
                        }
                        return EVAL(((MalList)a2).nth(2),
                                    new Env(env, ((MalList)a2).slice(1,2),
                                                 new MalList(exc)));
                    }
                }
                throw t;
            }
        case "do":
            for (int i=1; i<ast.size()-1; i++)
                EVAL(ast.nth(i), env);
            orig_ast = ast.nth(ast.size()-1);
            break;
        case "if":
            a1 = ast.nth(1);
            MalVal cond = EVAL(a1, env);
            if (cond == types.Nil || cond == types.False) {
                // eval false slot form
                if (ast.size() > 3) {
                    orig_ast = ast.nth(3);
                } else {
                    return types.Nil;
                }
            } else {
                // eval true slot form
                orig_ast = ast.nth(2);
            }
            break;
        case "fn*":
            final MalList a1f = (MalList)ast.nth(1);
            final MalVal a2f = ast.nth(2);
            final Env cur_env = env;
            return new MalFunction (a2f, (mal.env.Env)env, a1f) {
                public MalVal apply(MalList args) throws MalThrowable {
                    return EVAL(a2f, new Env(cur_env, a1f, args));
                }
            };
        default:
            final MalFunction f = (MalFunction)EVAL(a0, env);
            if (f.isMacro()) {
                orig_ast = f.apply(ast.rest());
                continue;
            }
            final MalList args = new MalList();
            for (int i=1; i<ast.size(); i++)
                args.conj_BANG(EVAL(ast.nth(i), env));
            MalVal fnast = f.getAst();
            if (fnast != null) {
                orig_ast = fnast;
                env = f.genEnv(args);
            } else {
                return f.apply(args);
            }
        }

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

        final Env repl_env = new Env(null);

        // core.java: defined using Java
        for (String key : core.ns.keySet()) {
            repl_env.set(new MalSymbol(key), core.ns.get(key));
        }
        repl_env.set(new MalSymbol("eval"), new MalFunction() {
            public MalVal apply(MalList args) throws MalThrowable {
                return EVAL(args.nth(0), repl_env);
            }
        });
        MalList _argv = new MalList();
        for (Integer i=1; i < args.length; i++) {
            _argv.conj_BANG(new MalString(args[i]));
        }
        repl_env.set(new MalSymbol("*ARGV*"), _argv);


        // core.mal: defined using the language itself
        RE(repl_env, "(def! not (fn* (a) (if a false true)))");
        RE(repl_env, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");
        RE(repl_env, "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
        
        Integer fileIdx = 0;
        if (args.length > 0 && args[0].equals("--raw")) {
            readline.mode = readline.Mode.JAVA;
            fileIdx = 1;
        }
        if (args.length > fileIdx) {
            RE(repl_env, "(load-file \"" + args[fileIdx] + "\")");
            return;
        }

        // repl loop
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
