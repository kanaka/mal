#!/usr/bin/env julia

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
function ispair(ast)
    (isa(ast, Array) || isa(ast, Tuple)) && length(ast) > 0
end

function quasiquote(ast)
    if !ispair(ast)
        [[:quote], Any[ast]]
    elseif ast[1] == :unquote
        ast[2]
    elseif ispair(ast[1]) && ast[1][1] == symbol("splice-unquote")
        [[:concat], Any[ast[1][2]], Any[quasiquote(ast[2:end])]]
    else
        [[:cons], Any[quasiquote(ast[1])], Any[quasiquote(ast[2:end])]]
    end
end

function ismacroCall(ast, env)
    return isa(ast, Array) &&
           isa(ast[1], Symbol) &&
           find(env, ast[1]) != nothing &&
           isa(get(env, ast[1]), MalFunc) &&
           get(env, ast[1]).ismacro
end

function macroexpand(ast, env)
    while ismacroCall(ast, env)
        mac = get(env, ast[1])
        ast = mac.fn(ast[2:end]...)
    end
    ast
end

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
  while true
    #println("EVAL: $(printer.pr_str(ast,true))")
    if !isa(ast, Array) return eval_ast(ast, env) end

    # apply
    ast = macroexpand(ast, env)
    if !isa(ast, Array) return ast end

    if     :def! == ast[1]
        return set(env, ast[2], EVAL(ast[3], env))
    elseif symbol("let*") == ast[1]
        let_env = Env(env)
        for i = 1:2:length(ast[2])
            set(let_env, ast[2][i], EVAL(ast[2][i+1], let_env))
        end
        env = let_env
        ast = ast[3]
        # TCO loop
    elseif :quote == ast[1]
        return ast[2]
    elseif :quasiquote == ast[1]
        ast = quasiquote(ast[2])
        # TCO loop
    elseif :defmacro! == ast[1]
        func = EVAL(ast[3], env)
        func.ismacro = true
        return set(env, ast[2], func)
    elseif :macroexpand == ast[1]
        return macroexpand(ast[2], env)
    elseif symbol("try*") == ast[1]
        try
            return EVAL(ast[2], env)
        catch exc
            e = string(exc)
            if isa(exc, MalException)
                e = exc.malval
            elseif isa(exc, ErrorException)
                e = exc.msg
            else
                e = string(e)
            end
            if length(ast) > 2 && ast[3][1] == symbol("catch*")
                return EVAL(ast[3][3], Env(env, {ast[3][2]}, {e}))
            else
                rethrow(exc)
            end
        end
    elseif :do == ast[1]
        eval_ast(ast[2:end-1], env)
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
            (args...) -> EVAL(ast[3], Env(env, ast[2], args)),
            ast[3], env, ast[2])
    else
        el = eval_ast(ast, env)
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
set(repl_env, :eval, (ast) -> EVAL(ast, repl_env))
set(repl_env, symbol("*ARGV*"), ARGS[2:end])

# core.mal: defined using the language itself
REP("(def! not (fn* (a) (if a false true)))")
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
REP("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")


if length(ARGS) > 0
    REP("(load-file \"$(ARGS[1])\")")
    exit(0)
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
