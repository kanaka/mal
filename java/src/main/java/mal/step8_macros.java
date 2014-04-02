package mal;

import java.io.IOException;
import java.io.FileNotFoundException;

import java.util.Scanner;
import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import mal.types.*;
import mal.readline;
import mal.reader;

public class step8_macros {
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
                                       quasiquote(types._rest((MalList)ast)));
                }
            }
            return new MalList(new MalSymbol("cons"),
                               quasiquote(a0),
                               quasiquote(types._rest((MalList)ast)));
        }
    }

    public static Boolean is_macro_call(MalVal ast, Env env)
            throws MalThrowable {
        if (ast instanceof MalList) {
            MalVal a0 = ((MalList)ast).nth(0);
            if (a0 instanceof MalSymbol &&
                env.find(((MalSymbol)a0).getName()) != null) {
                MalVal mac = env.get(((MalSymbol)a0).getName());
                if (mac instanceof MalFunction &&
                    ((MalFunction)mac).isMacro()) {
                    return true;
                }
            }
        }
        return false;
    }

    public static MalVal macroexpand(MalVal ast, Env env)
            throws MalThrowable {
        while (is_macro_call(ast, env)) {
            MalSymbol a0 = (MalSymbol)((MalList)ast).nth(0);
            MalFunction mac = (MalFunction) env.get(a0.getName());
            ast = mac.apply(types._rest((MalList)ast));
        }
        return ast;
    }

    public static MalVal eval_ast(MalVal ast, Env env) throws MalThrowable {
        if (ast instanceof MalSymbol) {
            MalSymbol sym = (MalSymbol)ast;
            return env.get(sym.getName());
        } else if (ast instanceof MalList) {
            MalList old_lst = (MalList)ast;
            MalList new_lst = types._list_Q(ast) ? new MalList()
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
        MalVal a1,a2, a3, res;
        MalList el;

        while (true) {

        //System.out.println("EVAL: " + types._pr_str(orig_ast, true));
        if (!(types._list_Q(orig_ast))) {
            return eval_ast(orig_ast, env);
        }

        // apply list
        MalVal expanded = macroexpand(orig_ast, env);
        if (!types._list_Q(expanded)) { return expanded; } 
        MalList ast = (MalList) expanded;
        if (ast.size() == 0) { return ast; }
        MalVal a0 = ast.nth(0);
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
        case "quote":
            return ast.nth(1);
        case "quasiquote":
            return EVAL(quasiquote(ast.nth(1)), env);
        case "defmacro!":
            a1 = ast.nth(1);
            a2 = ast.nth(2);
            res = EVAL(a2, env);
            ((MalFunction)res).setMacro();
            env.set(((MalSymbol)a1).getName(), res);
            return res;
        case "macroexpand":
            a1 = ast.nth(1);
            return macroexpand(a1, env);
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
            return new MalFunction (a2f, (mal.types.Env)env, a1f) {
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
                env = new Env(f.getEnv(), f.getParams(), el.slice(1));
            } else {
                return f.apply(types._rest(el));
            }
        }

        }
    }

    // print
    public static String PRINT(MalVal exp) {
        return types._pr_str(exp, true);
    }

    // REPL
    public static MalVal RE(Env env, String str) throws MalThrowable {
        return EVAL(READ(str), env);
    }
    public static Env _ref(Env env, String name, MalVal mv) {
        return env.set(name, mv);
    }
    public static String slurp(String fname) throws MalThrowable {
        try {
            return new Scanner(new File(fname))
                .useDelimiter("\\Z").next();
        } catch (FileNotFoundException e) {
            throw new MalError(e.getMessage());
        }
    }

    public static void main(String[] args) throws MalThrowable {
        String prompt = "user> ";

        final Env repl_env = new Env(null);
        for (String key : types.types_ns.keySet()) {
            _ref(repl_env, key, types.types_ns.get(key));
        }
        _ref(repl_env, "read-string", new MalFunction() {
            public MalVal apply(MalList args) throws MalThrowable {
                return reader.read_str(((MalString)args.nth(0)).getValue());
            }
        });
        _ref(repl_env, "eval", new MalFunction() {
            public MalVal apply(MalList args) throws MalThrowable {
                return EVAL(args.nth(0), repl_env);
            }
        });
        _ref(repl_env, "slurp", new MalFunction() {
            public MalVal apply(MalList args) throws MalThrowable {
                String fname = ((MalString)args.nth(0)).getValue();
                return new MalString(slurp(fname));
            }
        });

        RE(repl_env, "(def! not (fn* (a) (if a false true)))");
        RE(repl_env, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");
        
        Integer fileIdx = 0;
        if (args.length > 0 && args[0].equals("--raw")) {
            readline.mode = readline.Mode.JAVA;
            fileIdx = 1;
        }
        if (args.length > fileIdx) {
            for(Integer i=fileIdx; i<args.length; i++) {
                RE(repl_env, "(load-file \"" + args[i] + "\")");
            }
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
            } catch (reader.ParseError e) {
                System.out.println(e.getMessage());
                continue;
            } catch (MalThrowable t) {
                System.out.println("Error: " + t.getMessage());
                continue;
            }
        }
    }
}
