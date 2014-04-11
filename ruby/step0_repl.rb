require "readline"

# read
def READ(str)
    return str
end

# eval
def EVAL(ast, env)
    return ast
end

# print
def PRINT(exp)
    return exp
end

# repl
def REP(str)
    return PRINT(EVAL(READ(str), {}))
end

while line = Readline.readline("user> ", true)
    puts REP(line)
end
