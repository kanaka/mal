#!/usr/bin/env julia

push!(LOAD_PATH, pwd(), "/usr/share/julia/base")
import readline_mod
import reader
import printer
using env

# READ
function READ(str)
    reader.read_str(str)
end

# EVAL
function EVAL(ast, env)
    dbgenv = env_find(env, Symbol("DEBUG-EVAL"))
    if dbgenv != nothing
        dbgeval = env_get(dbgenv, Symbol("DEBUG-EVAL"))
        if dbgeval !== nothing && dbgeval !== false
            println("EVAL: $(printer.pr_str(ast,true))")
        end
    end

    if typeof(ast) == Symbol
        return env_get(env,ast)
    elseif isa(ast, Tuple)
        return map((x) -> EVAL(x,env), ast)
    elseif isa(ast, Dict)
        return [x[1] => EVAL(x[2], env) for x=ast]
    elseif !isa(ast, Array)
        return ast
    end

    if isempty(ast) return ast end

    # apply
    if     :def! == ast[1]
        env_set(env, ast[2], EVAL(ast[3], env))
    elseif symbol("let*") == ast[1]
        let_env = Env(env)
        for i = 1:2:length(ast[2])
            env_set(let_env, ast[2][i], EVAL(ast[2][i+1], let_env))
        end
        EVAL(ast[3], let_env)
    else
        el = map((x) -> EVAL(x,env), ast)
        f, args = el[1], el[2:end]
        f(args...)
    end
end

# PRINT
function PRINT(exp)
    printer.pr_str(exp)
end

# REPL
repl_env = Env(nothing,
               Dict{Any,Any}(:+ => +,
                             :- => -,
                             :* => *,
                             :/ => div))
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
