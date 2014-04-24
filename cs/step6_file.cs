using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using Mal;
using MalVal = Mal.types.MalVal;
using MalString = Mal.types.MalString;
using MalSymbol = Mal.types.MalSymbol;
using MalInteger = Mal.types.MalInteger;
using MalList = Mal.types.MalList;
using MalVector = Mal.types.MalVector;
using MalHashMap = Mal.types.MalHashMap;
using MalFunction = Mal.types.MalFunction;
using Env = Mal.env.Env;

namespace Mal {
    class step6_file {
        // read
        static MalVal READ(string str) {
            return reader.read_str(str);
        }

        // eval
        static MalVal eval_ast(MalVal ast, Env env) {
            if (ast is MalSymbol) {
                MalSymbol sym = (MalSymbol)ast;
                return env.get(sym.getName());
            } else if (ast is MalList) {
                MalList old_lst = (MalList)ast;
                MalList new_lst = ast.list_Q() ? new MalList()
                                            : (MalList)new MalVector();
                foreach (MalVal mv in old_lst.getValue()) {
                    new_lst.conj_BANG(EVAL(mv, env));
                }
                return new_lst;
            } else if (ast is MalHashMap) {
                var new_dict = new Dictionary<string, MalVal>();
                foreach (var entry in ((MalHashMap)ast).getValue()) {
                    new_dict.Add(entry.Key, EVAL((MalVal)entry.Value, env));
                }
                return new MalHashMap(new_dict);
            } else {
                return ast;
            }
        }


        static MalVal EVAL(MalVal orig_ast, Env env) {
            MalVal a0, a1, a2, res;
            MalList el;

            while (true) {

            //System.out.println("EVAL: " + printer._pr_str(orig_ast, true));
            if (!orig_ast.list_Q()) {
                return eval_ast(orig_ast, env);
            }

            // apply list
            MalList ast = (MalList)orig_ast;
            if (ast.size() == 0) { return ast; }
            a0 = ast[0];

            String a0sym = a0 is MalSymbol ? ((MalSymbol)a0).getName()
                                           : "__<*fn*>__";

            switch (a0sym) {
            case "def!":
                a1 = ast[1];
                a2 = ast[2];
                res = EVAL(a2, env);
                env.set(((MalSymbol)a1).getName(), res);
                return res;
            case "let*":
                a1 = ast[1];
                a2 = ast[2];
                MalSymbol key;
                MalVal val;
                Env let_env = new Env(env);
                for(int i=0; i<((MalList)a1).size(); i+=2) {
                    key = (MalSymbol)((MalList)a1)[i];
                    val = ((MalList)a1)[i+1];
                    let_env.set(key.getName(), EVAL(val, let_env));
                }
                orig_ast = a2;
                env = let_env;
                break;
            case "do":
                eval_ast(ast.slice(1, ast.size()-1), env);
                orig_ast = ast[ast.size()-1];
                break;
            case "if":
                a1 = ast[1];
                MalVal cond = EVAL(a1, env);
                if (cond == Mal.types.Nil || cond == Mal.types.False) {
                    // eval false slot form
                    if (ast.size() > 3) {
                        orig_ast = ast[3];
                    } else {
                        return Mal.types.Nil;
                    }
                } else {
                    // eval true slot form
                    orig_ast = ast[2];
                }
                break;
            case "fn*":
                MalList a1f = (MalList)ast[1];
                MalVal a2f = ast[2];
                Env cur_env = env;
                return new MalFunction(a2f, env, a1f,
                    args => EVAL(a2f, new Env(cur_env, a1f, args)) );
            default:
                el = (MalList)eval_ast(ast, env);
                var f = (MalFunction)el[0];
                MalVal fnast = f.getAst();
                if (fnast != null) {
                    orig_ast = fnast;
                    env = f.genEnv(el.rest());
                } else {
                    return f.apply(el.rest());
                }
                break;
            }

            }
        }

        // print
        static string PRINT(MalVal exp) {
            return printer._pr_str(exp, true);
        }

        // repl
        static MalVal RE(Env env, string str) {
            return EVAL(READ(str), env);
        }

        static void Main(string[] args) {
            string prompt = "user> ";
            
            // core.cs: defined using C#
            var repl_env = new env.Env(null);
            foreach (var entry in core.ns) {
                repl_env.set(entry.Key, entry.Value);
            }
            repl_env.set("eval", new MalFunction(a => EVAL(a[0], repl_env)));
            MalList _argv = new MalList();
            for (int i=1; i < args.Length; i++) {
                _argv.conj_BANG(new MalString(args[i]));
            }
            repl_env.set("*ARGV*", _argv);

            // core.mal: defined using the language itself
            RE(repl_env, "(def! not (fn* (a) (if a false true)))");
            RE(repl_env, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");

            int fileIdx = 0;
            if (args.Length > 0 && args[0] == "--raw") {
                Mal.readline.mode = Mal.readline.Mode.Raw;
                fileIdx = 1;
            }
            if (args.Length > fileIdx) {
                RE(repl_env, "(load-file \"" + args[fileIdx] + "\")");
                return;
            }
            
            // repl loop
            while (true) {
                string line;
                try {
                    line = Mal.readline.Readline(prompt);
                    if (line == null) { break; }
                } catch (IOException e) {
                    Console.WriteLine("IOException: " + e.Message);
                    break;
                }
                try {
                    Console.WriteLine(PRINT(RE(repl_env, line)));
                } catch (Mal.types.MalContinue) {
                    continue;
                } catch (Exception e) {
                    Console.WriteLine("Error: " + e.Message);
                    Console.WriteLine(e.StackTrace);
                    continue;
                }
            }
        }
    }
}
