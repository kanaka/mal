#!/usr/bin/env julia

import readline_mod
import reader
import printer
using env

# READ
function READ(str)
    reader.read_str(str)
end

# EVAL
function eval_ast(ast, env)
    if typeof(ast) == Symbol
        get(env,ast)
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

    # apply
    if     :def! == ast[1]
        set(env, ast[2], EVAL(ast[3], env))
    elseif symbol("let*") == ast[1]
        let_env = Env(env)
        for i = 1:2:length(ast[2])
            set(let_env, ast[2][i], EVAL(ast[2][i+1], let_env))
        end
        EVAL(ast[3], let_env)
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
repl_env = Env(nothing,
               {:+ => +,
                :- => -,
                :* => *,
                :/ => div})
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
