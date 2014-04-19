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
            string prompt = "user> ";

            while (true) {
                string line;
                try {
                    line = Mal.readline.Readline(prompt);
                    if (line == null) { break; }
                } catch (IOException e) {
                    Console.WriteLine("IOException: " + e.Message);
                    break;
                }
                Console.WriteLine(PRINT(RE(null, line)));
            }
        }
    }
}
