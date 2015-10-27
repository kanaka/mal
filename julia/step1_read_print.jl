#!/usr/bin/env julia

push!(LOAD_PATH, pwd(), "/usr/share/julia/base")
import readline_mod
import reader
import printer

# READ
function READ(str)
    reader.read_str(str)
end

# EVAL
function EVAL(ast, env)
    ast
end

# PRINT
function PRINT(exp)
    printer.pr_str(exp)
end

# REPL
function REP(str)
    return PRINT(EVAL(READ(str), []))
end

while true
    line = readline_mod.do_readline("user> ")
    if line === nothing break end
    try
        println(REP(line))
    catch e
        if isa(e, ErrorException)
            println("Error: $(e.msg)")
        else
            println("Error: $(string(e))")
        end
        bt = catch_backtrace()
        Base.show_backtrace(STDERR, bt)
        println()
    end
end
