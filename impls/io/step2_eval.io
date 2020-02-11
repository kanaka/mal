MalTypes
MalReader

READ := method(str, MalReader read_str(str))

eval_ast := method(ast, env,
   (ast type) switch(
       "MalSymbol", env at(ast val) ifNil(Exception raise("'" .. (ast val) "' not found")),
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
    el := eval_ast(ast, env)
    f := el at(0)
    args := el rest
    f callWithArgList(args)
)

PRINT := method(exp, exp malPrint(true))

repl_env := Map with(
    "+", block(a, b, a + b),
    "-", block(a, b, a - b),
    "*", block(a, b, a * b),
    "/", block(a, b, a / b)
)

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
