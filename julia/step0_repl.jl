#!/usr/bin/env julia

import readline_mod

# READ
function READ(str)
    str
end

# EVAL
function EVAL(ast, env)
    ast
end

# PRINT
function PRINT(exp)
    exp
end

# REPL
function REP(str)
    return PRINT(EVAL(READ(str), {}))
end

while true
    line = readline_mod.do_readline("user> ")
    if line === nothing break end
    println(REP(line))
end
