#!/usr/bin/env julia

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
    print("user> ")
    flush(STDOUT)
    line = readline(STDIN)
    if line == ""
        break
    end
    line = chomp(line)
    println(REP(line))
end
