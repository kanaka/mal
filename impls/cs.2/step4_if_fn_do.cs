using System;
using System.Collections.Generic;
using System.Linq;

namespace mal
{
    class step4_if_fn_do
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
                        else if (firstSymbol.value == "do")
                        {
                            List<MalType> evalAstd = astList.items.Skip(1).Select(item => EVAL(item, env)).ToList();
                            return evalAstd.Last();
                        }
                        else if (firstSymbol.value == "if")
                        {
                            MalType test = astList.items[1];
                            MalType testResult = EVAL(test, env);
                            if (testResult == MalNil.MAL_NIL || testResult == MalBoolean.MAL_FALSE)
                            {
                                return (astList.items.Count > 3) ? EVAL(astList.items[3], env) : MalNil.MAL_NIL;
                            }
                            else
                            {
                                return EVAL(astList.items[2], env);
                            }
                        }
                        else if (firstSymbol.value == "fn*")
                        {
                            MalSeq argNames = (MalSeq)astList.items[1];
                            MalType funcBody = astList.items[2];
                            List<MalSymbol> argSymbs = new List<MalSymbol>();
                            foreach (MalType arg in argNames.items) { if (arg is MalSymbol) argSymbs.Add((MalSymbol)arg); }
                            return new MalFunction(
                                (IList<MalType> argValues) =>
                                {
                                    Env funcEnv = new Env(env, argSymbs, argValues);
                                    return EVAL(funcBody, funcEnv);
                                }
                            );
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
            // Load the built-in functions
            foreach(var pair in core.ns) { repl_env.set(new MalSymbol(pair.Key), pair.Value); }

            // Functions defined on Mal itself
            rep("(def! not (fn* (a) (if a false true)))");

            // TESTS
            // var test = rep("(if (> 3 3) 1 2)");

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
