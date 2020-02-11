MalTypes
MalReader

READ := method(str, MalReader read_str(str))

EVAL := method(ast, env, ast)

PRINT := method(exp, exp malPrint(true))

RE := method(str, EVAL(READ(str), nil))

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
