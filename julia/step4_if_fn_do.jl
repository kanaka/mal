#!/usr/bin/env julia

push!(LOAD_PATH, pwd(), "/usr/share/julia/base")
import readline_mod
import reader
import printer
using env
import core

# READ
function READ(str)
    reader.read_str(str)
end

# EVAL
function eval_ast(ast, env)
    if typeof(ast) == Symbol
        env_get(env,ast)
    elseif isa(ast, Array) || isa(ast, Tuple)
        map((x) -> EVAL(x,env), ast)
    elseif isa(ast, Dict)
        [EVAL(x[1],env) => EVAL(x[2], env) for x=ast]
    else
        ast
    end
end

function EVAL(ast, env)
    if !isa(ast, Array) return eval_ast(ast, env) end
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
    elseif :do == ast[1]
        eval_ast(ast[2:end], env)[end]
    elseif :if == ast[1]
        cond = EVAL(ast[2], env)
        if cond === nothing || cond === false
            if length(ast) >= 4
                EVAL(ast[4], env)
            else
                nothing
            end
        else
            EVAL(ast[3], env)
        end
    elseif symbol("fn*") == ast[1]
        (args...) -> EVAL(ast[3], Env(env, ast[2], Any[args...]))
    else
        el = eval_ast(ast, env)
        f, args = el[1], el[2:end]
        f(args...)
    end
end

# PRINT
function PRINT(exp)
    printer.pr_str(exp)
end

# REPL
repl_env = nothing
function REP(str)
    return PRINT(EVAL(READ(str), repl_env))
end

# core.jl: defined using Julia
repl_env = Env(nothing, core.ns)

# core.mal: defined using the language itself
REP("(def! not (fn* (a) (if a false true)))")

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
        if !isa(e, StackOverflowError)
            bt = catch_backtrace()
            Base.show_backtrace(STDERR, bt)
        end
        println()
    end
end
