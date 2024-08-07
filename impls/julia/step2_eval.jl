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
    # println("EVAL: $(printer.pr_str(ast,true))")

    if typeof(ast) == Symbol
        return env[ast]
    elseif isa(ast, Tuple)
        return map((x) -> EVAL(x,env), ast)
    elseif isa(ast, Dict)
        return [x[1] => EVAL(x[2], env) for x=ast]
    elseif !isa(ast, Array)
        return ast
    end

    if isempty(ast) return ast end

    # apply
    el = map((x) -> EVAL(x,env), ast)
    f, args = el[1], el[2:end]
    f(args...)
end

# PRINT
function PRINT(exp)
    printer.pr_str(exp)
end

# REPL
repl_env = Dict{Any,Any}(:+ => +,
                         :- => -,
                         :* => *,
                         :/ => div)
function REP(str)
    return PRINT(EVAL(READ(str), repl_env))
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
