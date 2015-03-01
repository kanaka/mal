#!/usr/bin/env lua

local readline = require('readline')
local utils = require('utils')
local reader = require('reader')
local printer = require('printer')

-- read
function READ(str)
    return reader.read_str(str)
end

-- eval
function EVAL(ast, env)
    return ast
end

-- print
function PRINT(exp)
    return printer._pr_str(exp, true)
end

-- repl
function rep(str)
    return PRINT(EVAL(READ(str),""))
end

if #arg > 0 and arg[1] == "--raw" then
    readline.raw = true
end

while true do
    line = readline.readline("user> ")
    if not line then break end
    xpcall(function()
        print(rep(line))
    end, function(exc)
        if exc then
            if types._malexception_Q(exc) then
                exc = printer._pr_str(exc.val, true)
            end
            print("Error: " .. exc)
            print(debug.traceback())
        end
    end)
end
