MalTypes
MalReader

READ := method(str, MalReader read_str(str))

eval_ast := method(ast, env,
   (ast type) switch(
       "MalSymbol", env get(ast),
       "MalList", MalList with(ast map(a, EVAL(a, env))),
       "MalVector", MalVector with(ast map(a, EVAL(a, env))),
       "MalMap",
           m := MalMap clone
           ast foreach(k, v,
               keyObj := MalMap keyToObj(k)
               m atPut(MalMap objToKey(EVAL(keyObj, env)), EVAL(v, env))
           )
           m,
       ast
   )
)

EVAL := method(ast, env,
    if(ast type != "MalList", return(eval_ast(ast, env)))
    if(ast isEmpty, return ast)
    if(ast at(0) type == "MalSymbol",
        ast at(0) val switch(
            "def!",
                return(env set(ast at(1), EVAL(ast at(2), env))),
            "let*",
                letEnv := Env with(env)
                varName := nil
                ast at(1) foreach(i, e,
                    if(i % 2 == 0,
                        varName := e,
                        letEnv set(varName, EVAL(e, letEnv))
                    )
                )
                return(EVAL(ast at(2), letEnv))
        )
    )

    // Apply
    el := eval_ast(ast, env)
    f := el at(0)
    args := el rest
    f callWithArgList(args)
)

PRINT := method(exp, exp malPrint(true))

repl_env := Env with(nil)
repl_env set(MalSymbol with("+"), block(a, b, a + b))
repl_env set(MalSymbol with("-"), block(a, b, a - b))
repl_env set(MalSymbol with("*"), block(a, b, a * b))
repl_env set(MalSymbol with("/"), block(a, b, a / b))

RE := method(str, EVAL(READ(str), repl_env))

REP := method(str, PRINT(RE(str)))

loop(
    line := MalReadline readLine("user> ")
    if(line isNil, break)
    if(line isEmpty, continue)
    e := try(REP(line) println)
    e catch(Exception,
        ("Error: " .. (e error)) println
    )
)
