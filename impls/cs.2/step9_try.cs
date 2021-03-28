using System;
using System.Collections.Generic;
using System.Linq;

namespace mal
{
    class step9_try
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
                        // Macroexpand before special forms
                        MalType expanded = macroexpand(astList, env);
                        if (expanded is not MalList) return eval_ast(expanded, env);
                        else
                        {
                            ast = (MalList)expanded;
                            astList = (MalList)expanded;
                        }

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
                            if (firstSymbol.value == "defmacro!")
                            {
                                MalSymbol symbol = (MalSymbol)astList.items[1];
                                MalType value = EVAL(astList.items[2], env);
                                if (value is MalFunction) ((MalFunction)value).is_macro = true;
                                if (value is MalFnTco) ((MalFnTco)value).is_macro = true;
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
                            else if (firstSymbol.value == "quote")
                            {
                                return astList.items[1];
                            }
                            else if (firstSymbol.value == "quasiquoteexpand")
                            {
                                return quasiquote(astList.items[1]);
                            }
                            else if (firstSymbol.value == "quasiquote")
                            {
                                ast = quasiquote(astList.items[1]);
                                continue;
                            }
                            else if (firstSymbol.value == "macroexpand")
                            {
                                return macroexpand(astList.items[1], env);
                            }
                            else if (firstSymbol.value == "try*")
                            {
                                try
                                {
                                    return EVAL(astList.items[1], env);
                                }
                                catch (MalException exception)
                                {
                                    // if a catch block exists, create a MalException and evaluate the expression of the block with it
                                    if (astList.items.Count > 2 && astList.items[2] is MalList)
                                    {
                                        MalList catchArgs = (MalList)astList.items[2];
                                        MalSymbol exName = (MalSymbol)catchArgs.items[1];
                                        MalType expr = catchArgs.items[2];
                                        Env exEnv = new Env(env);
                                        exEnv.set(exName, exception.cause);
                                        return EVAL(expr, exEnv);
                                    }
                                    else throw new MalException(exception.cause); // otherwise just rethrow
                                }
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

        static bool is_macro_call(MalType ast, Env env)
        {
            if (ast is MalList)
            {
                MalType head = ((MalList)ast).items[0];
                if (head is MalSymbol && env.find((MalSymbol)head) != null)
                {
                    MalType value = env.get((MalSymbol)head);
                    if (value is MalFunction) return ((MalFunction)value).is_macro;
                    if (value is MalFnTco) return ((MalFnTco)value).is_macro;
                }
            }
            return false;
        }

        static MalType macroexpand(MalType ast, Env env)
        {
            bool expanding;
            while (expanding = is_macro_call(ast, env))
            {
                MalSymbol name = (MalSymbol)((MalSeq)ast).items[0];
                List<MalType> args = ((MalSeq)ast).items.Skip(1).ToList();
                MalType macroFn = env.get(name);
                if (macroFn is MalFunction) ast = ((MalFunction)macroFn).function(args);
                if (macroFn is MalFnTco) ast = ((MalFnTco)macroFn).fn.function(args);
            }
            return ast;
        }

        static MalType quasiquote(MalType ast)
        {
            if (ast is MalSeq)
            {
                MalSeq astSeq = (MalSeq)ast;
                if (ast is MalList && astSeq.items.Count > 0)
                {
                    MalType head = astSeq.items[0];
                    if (head is MalSymbol && ((MalSymbol)head).value == "unquote")
                    {
                        return astSeq.items[1];
                    }
                }

                List<MalType> result = new List<MalType>();

                // Iterate over each element elt of ast in reverse order
                for (int i = astSeq.items.Count - 1; i >= 0; i--)
                {
                    MalType elt = astSeq.items[i];

                    // If elt is a list starting with the "splice-unquote" symbol
                    if (elt is MalList && ((MalList)elt).items.Count >= 2 && ((MalList)elt).items[0].Equals(new MalSymbol("splice-unquote")))
                    {
                        // replace the current result with a list containing: the "concat" symbol,
                        // the second element of elt, then the previous result.
                        result = new List<MalType>(){
                            new MalSymbol("concat"),
                            ((MalList)elt).items[1],
                            new MalList(result)
                        };
                    }
                    else
                    {
                        // replace the current result with a list containing: the "cons" symbol,
                        // the result of calling quasiquote with elt as argument, then the previous result
                        result = new List<MalType>()
                        {
                            new MalSymbol("cons"),
                            quasiquote(elt),
                            new MalList(result)
                        };
                    }
                }

                if (ast is MalVector)
                {
                    // when ast is a vector, return a list containing: the "vec" symbol,
                    // then the result of processing ast as if it were a list not starting with quote
                    result = new List<MalType>() { new MalSymbol("vec"), new MalList(result) };
                }

                return new MalList(result);
            }
            else if (ast is MalHashmap || ast is MalSymbol)
            {
                // If ast is a map or a symbol, return a list containing: the "quote" symbol, then ast
                return new MalList(new List<MalType>() { new MalSymbol("quote"), ast });
            }
            else return ast; // Else return ast unchanged
        }

        static string PRINT(MalType input)
        {
            return printer.pr_str(input, true);
        }

        static string rep(string input)
        {
            MalType read = READ(input);
            MalType evaled;
            try
            {
                evaled = EVAL(read, repl_env);
                if (evaled == null) return null;
                string printed = PRINT(evaled);
                return printed;
            }
            catch (MalException mex)
            {
                return string.Format("Exception: {0}", printer.pr_str(mex.cause));
            }
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
            rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");

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
            // rep("`[unquote 0]");

            string line = null;
            do
            {
                Console.Write("user> ");
                line = Console.ReadLine();
                if (line != null)
                {
                    try
                    {
                        string repResult = rep(line);
                        if (repResult != null) Console.WriteLine(repResult);
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
