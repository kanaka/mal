using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using Mal;
using MalVal = Mal.types.MalVal;
using MalSymbol = Mal.types.MalSymbol;
using MalInteger = Mal.types.MalInteger;
using MalList = Mal.types.MalList;
using MalVector = Mal.types.MalVector;
using MalHashMap = Mal.types.MalHashMap;
using MalFunction = Mal.types.MalFunction;
using Env = Mal.env.Env;

namespace Mal {
    class step3_env {
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
            //System.out.println("EVAL: " + printer._pr_str(orig_ast, true));
            if (!orig_ast.list_Q()) {
                return eval_ast(orig_ast, env);
            }

            // apply list
            MalList ast = (MalList)orig_ast;
            if (ast.size() == 0) { return ast; }
            a0 = ast[0];
            if (!(a0 is MalSymbol)) {
                throw new Mal.types.MalError("attempt to apply on non-symbol '"
                        + Mal.printer._pr_str(a0,true) + "'");
            }

            switch (((MalSymbol)a0).getName()) {
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
                return EVAL(a2, let_env);
            default:
                el = (MalList)eval_ast(ast, env);
                var f = (MalFunction)el[0];
                return f.apply(el.rest());
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

        static public MalFunction plus = new MalFunction(
                a => (MalInteger)a[0] + (MalInteger)a[1] );
        static public MalFunction minus = new MalFunction(
                a => (MalInteger)a[0] - (MalInteger)a[1] );
        static public MalFunction multiply = new MalFunction(
                a => (MalInteger)a[0] * (MalInteger)a[1] );
        static public MalFunction divide = new MalFunction(
                a => (MalInteger)a[0] / (MalInteger)a[1] );


        static void Main(string[] args) {
            string prompt = "user> ";
            
            var repl_env = new Mal.env.Env(null);
            repl_env.set("+", plus);
            repl_env.set("-", minus);
            repl_env.set("*", multiply);
            repl_env.set("/", divide);

            if (args.Length > 0 && args[0] == "--raw") {
                Mal.readline.mode = Mal.readline.Mode.Raw;
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
