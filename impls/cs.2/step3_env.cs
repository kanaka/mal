using System;
using System.Collections.Generic;
using System.Linq;

namespace mal
{
    class step3_env
    {

        static MalType READ(string input)
        {
            return Reader.read_str(input);
        }

        public static MalType EVAL(MalType ast, Env env)
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

                    MalType first = astList.items.First();
                    if (first is MalSymbol)
                    {
                        MalSymbol firstSymbol = (MalSymbol)first;
                        if (firstSymbol.value == "def!")
                        {
                            MalSymbol symbol = (MalSymbol)astList.items[1];
                            MalType value = EVAL(astList.items[2], env);
                            env.set(symbol, value);
                            return value;
                        }
                        else if (firstSymbol.value == "let*")
                        {
                            MalSeq bindings = (MalSeq)astList.items[1];
                            MalType expression = astList.items[2];
                            Env newEnv = new Env(env);
                            for (int i = 0; i < bindings.items.Count; i += 2)
                            {
                                MalSymbol name = (MalSymbol)bindings.items[i];
                                MalType value = EVAL(bindings.items[i + 1], newEnv);
                                newEnv.data.Add(name, value);
                            }

                            return EVAL(expression, newEnv);
                        }

                    }

                    // Function application
                    MalList evaluated = (MalList)(eval_ast(ast, env));
                    MalFunction func = (MalFunction)evaluated.items[0];
                    MalType retVal = func.function(evaluated.items.Skip(1).ToList());
                    return retVal;
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

        public static MalType eval_ast(MalType ast, Env env)
        {
            if (ast is MalSymbol)
            {
                MalSymbol astSymbol = (MalSymbol)ast;
                return env.get(astSymbol);
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
                    newKVs.Add(EVAL(kv.Key, env), EVAL(kv.Value, env));
                }
                return new MalHashmap(newKVs);
            }
            else
            {
                return ast;
            }
        }

        public static Env repl_env = new Env(null);

        static void Main(string[] args)
        {
            repl_env.set(
                new MalSymbol("+"),
                new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value + ((MalInteger)args[1]).value))
            );
            repl_env.set(
                new MalSymbol("-"),
                new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value - ((MalInteger)args[1]).value))
            );
            repl_env.set(
                new MalSymbol("*"),
                new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value * ((MalInteger)args[1]).value))
            );
            repl_env.set(
                new MalSymbol("/"),
                new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value / ((MalInteger)args[1]).value))
            );

            // TESTS
            // var test = rep("(let* (c 2) (+ 1 c))");
            rep("(let* [z 9] z)");

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
