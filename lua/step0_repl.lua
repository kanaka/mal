#!/usr/bin/env lua

local readline = require('readline')

function READ(str)
    return str
end

function EVAL(ast, any)
    return ast
end

function PRINT(exp)
    return exp
end

function rep(str)
    return PRINT(EVAL(READ(str),""))
end

if #arg > 0 and arg[1] == "--raw" then
    readline.raw = true
end

while true do
    line = readline.readline("user> ")
    if not line then break end
    print(rep(line))
end
