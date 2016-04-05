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
            "do",
                return(eval_ast(ast rest, env) last),
            "if",
                return(EVAL(if(EVAL(ast at(1), env), ast at(2), ast at(3)), env)),
            "fn*",
                return(block(a, EVAL(ast at(2), Env with(env, ast at(1), a)))),
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
    f call(args)
)

PRINT := method(exp, exp malPrint(true))

RE := method(str, EVAL(READ(str), repl_env))

REP := method(str, PRINT(RE(str)))

repl_env := Env with(nil)
MalCore NS foreach(k, v, repl_env set(MalSymbol with(k), v))

// core.mal: defined using the language itself
RE("(def! not (fn* (a) (if a false true)))")

loop(
    line := MalReadline readLine("user> ")
    if(line isNil, break)
    if(line isEmpty, continue)
    e := try(REP(line) println)
    e catch(Exception,
        ("Error: " .. (e error)) println
    )
)
