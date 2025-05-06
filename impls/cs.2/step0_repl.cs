using System;

namespace mal
{
    class step0_repl
    {

        static string READ(string input)
        {
            return input;
        }

        static string EVAL(string input)
        {
            return input;
        }

        static string PRINT(string input)
        {
            return input;
        }

        static string rep(string input)
        {
            string read = READ(input);
            string evaled = EVAL(read);
            string printed = PRINT(evaled);
            return printed;
        }

        static void Main(string[] args)
        {

            string line = null;
            do
            {
                Console.Write("user> ");
                line = Console.ReadLine();
                if (line != null)
                {
                    Console.WriteLine(rep(line));
                }
            } while (line != null);
        }
    }
}

