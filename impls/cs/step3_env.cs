using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using Mal;
using MalVal = Mal.types.MalVal;
using MalSymbol = Mal.types.MalSymbol;
using MalInt = Mal.types.MalInt;
using MalList = Mal.types.MalList;
using MalVector = Mal.types.MalVector;
using MalHashMap = Mal.types.MalHashMap;
using MalFunc = Mal.types.MalFunc;
using Env = Mal.env.Env;

namespace Mal {
    class step3_env {
        // read
        static MalVal READ(string str) {
            return reader.read_str(str);
        }

        // eval
        static MalVal EVAL(MalVal orig_ast, Env env) {
            MalVal a0, a1, a2, res;
            MalVal dbgeval = env.get("DEBUG-EVAL");
            if (dbgeval != null && dbgeval != Mal.types.Nil
                && dbgeval != Mal.types.False)
                Console.WriteLine("EVAL: " + printer._pr_str(orig_ast, true));
            if (orig_ast is MalSymbol) {
                string key = ((MalSymbol)orig_ast).getName();
                res = env.get(key);
                if (res == null)
                    throw new Mal.types.MalException("'" + key + "' not found");
                return res;
            } else if (orig_ast is MalVector) {
                MalVector old_lst = (MalVector)orig_ast;
                MalVector new_lst = new MalVector();
                foreach (MalVal mv in old_lst.getValue()) {
                    new_lst.conj_BANG(EVAL(mv, env));
                }
                return new_lst;
            } else if (orig_ast is MalHashMap) {
                var new_dict = new Dictionary<string, MalVal>();
                foreach (var entry in ((MalHashMap)orig_ast).getValue()) {
                    new_dict.Add(entry.Key, EVAL((MalVal)entry.Value, env));
                }
                return new MalHashMap(new_dict);
            } else if (!(orig_ast is MalList)) {
                return orig_ast;
            }

            // apply list
            MalList ast = (MalList) orig_ast;

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
                env.set((MalSymbol)a1, res);
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
                    let_env.set(key, EVAL(val, let_env));
                }
                return EVAL(a2, let_env);
            default:
                MalFunc f = (MalFunc)EVAL(ast[0], env);
                MalList arguments = new MalList();
                foreach (MalVal mv in ast.rest().getValue()) {
                    arguments.conj_BANG(EVAL(mv, env));
                }
                return f.apply(arguments);
            }
        }

        // print
        static string PRINT(MalVal exp) {
            return printer._pr_str(exp, true);
        }

        // repl
        static void Main(string[] args) {
            var repl_env = new Mal.env.Env(null);
            Func<string, MalVal> RE = (string str) => EVAL(READ(str), repl_env);
            repl_env.set(new MalSymbol("+"), new MalFunc(
                        a => (MalInt)a[0] + (MalInt)a[1]) );
            repl_env.set(new MalSymbol("-"), new MalFunc(
                        a => (MalInt)a[0] - (MalInt)a[1]) );
            repl_env.set(new MalSymbol("*"), new MalFunc(
                        a => (MalInt)a[0] * (MalInt)a[1]) );
            repl_env.set(new MalSymbol("/"), new MalFunc(
                        a => (MalInt)a[0] / (MalInt)a[1]) );

            if (args.Length > 0 && args[0] == "--raw") {
                Mal.readline.mode = Mal.readline.Mode.Raw;
            }

            // repl loop
            while (true) {
                string line;
                try {
                    line = Mal.readline.Readline("user> ");
                    if (line == null) { break; }
                    if (line == "") { continue; }
                } catch (IOException e) {
                    Console.WriteLine("IOException: " + e.Message);
                    break;
                }
                try {
                    Console.WriteLine(PRINT(RE(line)));
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
