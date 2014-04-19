using System;
using System.IO;
using Mal;
using MalVal = Mal.types.MalVal;

namespace Mal {
    class step1_read_print {
        // read
        static MalVal READ(string str) {
            return reader.read_str(str);
        }

        // eval
        static MalVal EVAL(MalVal ast, string env) {
            return ast;
        }

        // print
        static string PRINT(MalVal exp) {
            return printer._pr_str(exp, true);
        }

        // repl
        static MalVal RE(string env, string str) {
            return EVAL(READ(str), env);
        }

        static void Main(string[] args) {
            string prompt = "user> ";
            
            if (args.Length > 0 && args[0] == "--raw") {
                Mal.readline.mode = Mal.readline.Mode.Raw;
            }
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
                    Console.WriteLine(PRINT(RE(null, line)));
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
