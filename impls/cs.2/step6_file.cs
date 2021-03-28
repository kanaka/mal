using System;
using System.Collections.Generic;
using System.Linq;

namespace mal
{
    class step6_file
    {

        static MalType READ(string input)
        {
            return Reader.read_str(input);
        }

        public static MalType EVAL(MalType ast, Env env)
        {
            while (true)
            {
                if (ast is not MalList)
                {
                    return eval_ast(ast, env);
                }
                else
                {
                    MalList astList = (MalList)ast;

                    // Remove any comments from the ast
                    List<MalType> nonComments = astList.items.Where(it => !(it is MalSymbol && ((MalSymbol)it).value.StartsWith(";"))).ToList();
                    astList = new MalList(nonComments);

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
                                env = newEnv;
                                ast = expression;
                                continue;
                            }
                            else if (firstSymbol.value == "do")
                            {
                                int butLast = astList.items.Count - 2;

                                // produce side-effects and then just forget
                                List<MalType> sideEffects = astList.items.Skip(1).Take(butLast).ToList();
                                eval_ast(new MalList(sideEffects), env);

                                ast = astList.items.Last();
                                continue;
                            }
                            else if (firstSymbol.value == "if")
                            {
                                MalType test = astList.items[1];
                                MalType testResult = EVAL(test, env);
                                if (testResult == MalNil.MAL_NIL || testResult == MalBoolean.MAL_FALSE)
                                {
                                    ast = (astList.items.Count > 3) ? astList.items[3] : MalNil.MAL_NIL;
                                }
                                else
                                {
                                    ast = astList.items[2];
                                }
                                continue;
                            }
                            else if (firstSymbol.value == "fn*")
                            {
                                MalSeq argNames = (MalSeq)astList.items[1];
                                MalType funcBody = astList.items[2];
                                List<MalSymbol> argSymbs = new List<MalSymbol>();
                                foreach (MalType arg in argNames.items) { if (arg is MalSymbol) argSymbs.Add((MalSymbol)arg); }
                                MalFunction fn = new MalFunction(
                                    (IList<MalType> argValues) =>
                                    {
                                        Env funcEnv = new Env(env, argSymbs, argValues);
                                        return EVAL(funcBody, funcEnv);
                                    }
                                );
                                return new MalFnTco(funcBody, argSymbs, env, fn);
                            }

                        }

                        MalType evaluated = eval_ast(ast, env);
                        if (evaluated is not MalList) return evaluated;
                        MalList evaluatedList = (MalList)evaluated;

                        // Function application
                        MalType funcFirst = evaluatedList.items[0];
                        if (funcFirst is MalFunction)
                        {
                            MalFunction func = (MalFunction)funcFirst;
                            MalType retVal = func.function(evaluatedList.items.Skip(1).ToList());
                            return retVal;
                        }
                        else // MalFnTco
                        {
                            MalFnTco fnTco = (MalFnTco)funcFirst;
                            List<MalType> fnArgs = evaluatedList.items.Skip(1).ToList();
                            Env newEnv = new Env(fnTco.env, fnTco.@params, fnArgs);
                            ast = fnTco.ast;
                            env = newEnv;
                            continue;
                        }
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
            foreach (var pair in core.ns) { repl_env.set(new MalSymbol(pair.Key), pair.Value); }

            // Define 'eval'
            repl_env.data.Add(new MalSymbol("eval"), new MalFunction((IList<MalType> args) =>
            {
                MalType ast = args[0];
                return EVAL(ast, repl_env);
            }));

            // Functions defined on Mal itself:
            // - define 'not'
            rep("(def! not (fn* (a) (if a false true)))");
            // - define 'load-file'
            rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\"))))))");

            // ARGV
            List<MalType> malArgs = new List<MalType>();
            malArgs.AddRange(args.Skip(1).Select(arg => new MalString(arg)).ToList());
            repl_env.data.Add(new MalSymbol("*ARGV*"), new MalList(malArgs));

            // if called with arguments, treat first as a script name
            if (args.Length > 0)
            {
                rep("(load-file \"" + args[0] + "\")");
                return;
            }

            // TESTS
            // rep("(load-file \"../../tests/computations.mal\")");

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
