using System;

namespace mal
{
    class step1_read_print
    {

        static MalType READ(string input)
        {
            return Reader.read_str(input);
        }

        static MalType EVAL(MalType input)
        {
            return input;
        }

        static string PRINT(MalType input)
        {
            return printer.pr_str(input, true);
        }

        static string rep(string input)
        {
            MalType read = READ(input);
            MalType evaled = EVAL(read);
            string printed = PRINT(evaled);
            return printed;
        }

        static void Main(string[] args)
        {
            // TESTS
            // var test = rep("^{a 1} [1 2 3]");
            rep("[+ 1 2]");

            string line = null;
            do
            {
                Console.Write("user> ");
                line = Console.ReadLine();
                if (line != null)
                {
                    try
                    {
                        Console.WriteLine(rep(line));
                    }
                    catch (MalException mex)
                    {

                        Console.WriteLine(mex.cause);
                    }
                    catch (Exception ex)
                    {

                        Console.WriteLine(ex.Message);
                    }
                }
            } while (line != null);
            Console.WriteLine();
        }
    }
}
