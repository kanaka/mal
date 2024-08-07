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

namespace Mal {
    class step2_eval {
        // read
        static MalVal READ(string str) {
            return reader.read_str(str);
        }

        // eval
        static MalVal EVAL(MalVal orig_ast, Dictionary<string, MalVal> env) {
            MalVal a0;
            // Console.WriteLine("EVAL: " + printer._pr_str(orig_ast, true));
            if (orig_ast is MalSymbol) {
                MalSymbol sym = (MalSymbol)orig_ast;
                return (MalVal)env[sym.getName()];
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
            MalFunc f = (MalFunc)EVAL(ast[0], env);
            MalList arguments = new MalList();
            foreach (MalVal mv in ast.rest().getValue()) {
                arguments.conj_BANG(EVAL(mv, env));
            }
            return f.apply(arguments);
        }

        // print
        static string PRINT(MalVal exp) {
            return printer._pr_str(exp, true);
        }

        // repl
        static void Main(string[] args) {
            var repl_env = new Dictionary<string, MalVal> {
                {"+", new MalFunc(a => (MalInt)a[0] + (MalInt)a[1]) },
                {"-", new MalFunc(a => (MalInt)a[0] - (MalInt)a[1]) },
                {"*", new MalFunc(a => (MalInt)a[0] * (MalInt)a[1]) },
                {"/", new MalFunc(a => (MalInt)a[0] / (MalInt)a[1]) },
            };
            Func<string, MalVal> RE = (string str) => EVAL(READ(str), repl_env);

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
