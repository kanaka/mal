#!/usr/bin/env julia

push!(LOAD_PATH, pwd(), "/usr/share/julia/base")
import readline_mod
import reader
import printer
using env
import core
using types

# READ
function READ(str)
    reader.read_str(str)
end

# EVAL
function EVAL(ast, env)
  while true

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
        return env_set(env, ast[2], EVAL(ast[3], env))
    elseif symbol("let*") == ast[1]
        let_env = Env(env)
        for i = 1:2:length(ast[2])
            env_set(let_env, ast[2][i], EVAL(ast[2][i+1], let_env))
        end
        env = let_env
        ast = ast[3]
        # TCO loop
    elseif :do == ast[1]
        map((x) -> EVAL(x,env), ast[2:end-1])
        ast = ast[end]
        # TCO loop
    elseif :if == ast[1]
        cond = EVAL(ast[2], env)
        if cond === nothing || cond === false
            if length(ast) >= 4
                ast = ast[4]
                # TCO loop
            else
                return nothing
            end
        else
            ast = ast[3]
            # TCO loop
        end
    elseif symbol("fn*") == ast[1]
        return MalFunc(
            (args...) -> EVAL(ast[3], Env(env, ast[2], Any[args...])),
            ast[3], env, ast[2])
    else
        el = map((x) -> EVAL(x,env), ast)
        f, args = el[1], el[2:end]
        if isa(f, MalFunc)
            ast = f.ast
            env = Env(f.env, f.params, args)
            # TCO loop
        else
            return f(args...)
        end
    end
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
        # TODO: show at least part of stack
        if !isa(e, StackOverflowError)
            bt = catch_backtrace()
            Base.show_backtrace(STDERR, bt)
        end
        println()
    end
end
