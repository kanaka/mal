READ := method(str, str)

EVAL := method(ast, env, ast)

PRINT := method(exp, exp)

RE := method(str, EVAL(READ(str), nil))

REP := method(str, PRINT(RE(str)))

loop(
    line := MalReadline readLine("user> ")
    if(line isNil, break)
    if(line isEmpty, continue)
    REP(line) println
)
