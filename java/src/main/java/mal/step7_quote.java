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

public class step7_quote {
    // read
    public static MalVal READ(String str) throws MalThrowable {
        return reader.read_str(str);
    }

    // eval
    public static Boolean is_pair(MalVal x) {
        return x instanceof MalList && ((MalList)x).size() > 0;
    }

    public static MalVal quasiquote(MalVal ast) {
        if (!is_pair(ast)) {
            return new MalList(new MalSymbol("quote"), ast);
        } else {
            MalVal a0 = ((MalList)ast).nth(0);
            if ((a0 instanceof MalSymbol) &&
                (((MalSymbol)a0).getName() == "unquote")) {
                return ((MalList)ast).nth(1);
            } else if (is_pair(a0)) {
                MalVal a00 = ((MalList)a0).nth(0);
                if ((a00 instanceof MalSymbol) &&
                    (((MalSymbol)a00).getName() == "splice-unquote")) {
                    return new MalList(new MalSymbol("concat"),
                                       ((MalList)a0).nth(1),
                                       quasiquote(((MalList)ast).rest()));
                }
            }
            return new MalList(new MalSymbol("cons"),
                               quasiquote(a0),
                               quasiquote(((MalList)ast).rest()));
        }
    }

    public static MalVal eval_ast(MalVal ast, Env env) throws MalThrowable {
        if (ast instanceof MalSymbol) {
            return env.get((MalSymbol)ast);
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

        while (true) {

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
        case "do":
            eval_ast(ast.slice(1, ast.size()-1), env);
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
            el = (MalList)eval_ast(ast, env);
            MalFunction f = (MalFunction)el.nth(0);
            MalVal fnast = f.getAst();
            if (fnast != null) {
                orig_ast = fnast;
                env = f.genEnv(el.slice(1));
            } else {
                return f.apply(el.rest());
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
        RE(repl_env, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");
        
        Integer fileIdx = 0;
        if (args.length > 0 && args[0].equals("--raw")) {
            readline.mode = readline.Mode.JAVA;
            fileIdx = 1;
        }
        if (args.length > fileIdx) {
            RE(repl_env, "(load-file \"" + args[fileIdx] + "\")");
            return;
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
