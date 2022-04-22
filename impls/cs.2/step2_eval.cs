using System;
using System.Collections.Generic;
using System.Linq;

namespace mal
{
    class step2_eval
    {

        static MalType READ(string input)
        {
            return Reader.read_str(input);
        }

        public static MalType EVAL(MalType ast, Dictionary<string, MalFunction> env)
        {
            if (ast is not MalList)
            {
                return eval_ast(ast, env);
            }
            else
            {
                MalList astList = (MalList)ast;
                if (astList.items.Count == 0)
                {
                    return astList;
                }
                else
                {
                    MalList evaluated = (MalList)(eval_ast(ast, env));
                    if (astList is MalList)
                    {
                        MalFunction func = (MalFunction)evaluated.items[0];
                        MalType retVal = func.function(evaluated.items.Skip(1).ToList());
                        return retVal;
                    }
                    else
                    {
                        return evaluated; // vector
                    }
                }
            }
        }

        static string PRINT(MalType input)
        {
            return printer.pr_str(input, true);
        }

        static string rep(string input)
        {
            MalType read = READ(input);
            MalType evaled = EVAL(read, repl_env);
            string printed = PRINT(evaled);
            return printed;
        }

        public static MalType eval_ast(MalType ast, Dictionary<string, MalFunction> env)
        {
            if (ast is MalSymbol)
            {
                MalSymbol astSymbol = (MalSymbol)ast;
                if (env.ContainsKey(astSymbol.value))
                {
                    return env.GetValueOrDefault(astSymbol.value);
                }
                else
                {
                    throw new Exception(string.Format("Unknown symbol: {0}", astSymbol.value));
                }
            }
            else if (ast is MalList)
            {
                MalList astList = (MalList)ast;
                List<MalType> evaluated = astList.items.Select(item => EVAL(item, env)).ToList();
                return new MalList(evaluated);
            }
            else if (ast is MalVector)
            {
                MalVector astVector = (MalVector)ast;
                List<MalType> evaluated = astVector.items.Select(item => EVAL(item, env)).ToList();
                return new MalVector(evaluated);
            }
            else if (ast is MalHashmap)
            {
                MalHashmap astHashmap = (MalHashmap)ast;
                Dictionary<MalType, MalType> newKVs = new Dictionary<MalType, MalType>();
                foreach (KeyValuePair<MalType, MalType> kv in astHashmap.values)
                {
                    newKVs.Add(EVAL(kv.Key, repl_env), EVAL(kv.Value, repl_env));
                }
                return new MalHashmap(newKVs);
            }
            else
            {
                return ast;
            }
        }

        public static Dictionary<string, MalFunction> repl_env = new Dictionary<string, MalFunction>()
        {
            // {"+", (IList<MalType> args) => new MalInteger( args.Select(n => ((MalInteger)n).value).Sum() ) },
            {"+", new MalFunction(
                (IList<MalType> args) => new MalInteger( ((MalInteger) args[0]).value + ((MalInteger) args[1]).value )
            )},
            {"-", new MalFunction(
                (IList<MalType> args) => new MalInteger( ((MalInteger) args[0]).value - ((MalInteger) args[1]).value )
            )},
            {"*", new MalFunction(
                (IList<MalType> args) => new MalInteger( ((MalInteger) args[0]).value * ((MalInteger) args[1]).value )
            )},
            {"/", new MalFunction(
                (IList<MalType> args) => new MalInteger( ((MalInteger) args[0]).value / ((MalInteger) args[1]).value )
            )},
        };

        static void Main(string[] args)
        {
            // TESTS
            // var test = rep("[0 (+ 1 2)]");

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
