using System;
using System.IO;
using Mal;

namespace Mal {
    class step0_repl {
        // read
        static string READ(string str) {
            return str;
        }

        // eval
        static string EVAL(string ast, string env) {
            return ast;
        }

        // print
        static string PRINT(string exp) {
            return exp;
        }

        // repl
        static string RE(string env, string str) {
            return EVAL(READ(str), env);
        }

        static void Main(string[] args) {
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
                Console.WriteLine(PRINT(RE(null, line)));
            }
        }
    }
}
