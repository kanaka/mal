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
function ispair(ast)
    (isa(ast, Array) || isa(ast, Tuple)) && length(ast) > 0
end

function quasiquote(ast)
    if !ispair(ast)
        [[:quote]; Any[ast]]
    elseif ast[1] == :unquote
        ast[2]
    elseif ispair(ast[1]) && ast[1][1] == symbol("splice-unquote")
        [[:concat]; Any[ast[1][2]]; Any[quasiquote(ast[2:end])]]
    else
        [[:cons]; Any[quasiquote(ast[1])]; Any[quasiquote(ast[2:end])]]
    end
end

function ismacroCall(ast, env)
    return isa(ast, Array) &&
           isa(ast[1], Symbol) &&
           env_find(env, ast[1]) != nothing &&
           isa(env_get(env, ast[1]), MalFunc) &&
           env_get(env, ast[1]).ismacro
end

function macroexpand(ast, env)
    while ismacroCall(ast, env)
        mac = env_get(env, ast[1])
        ast = mac.fn(ast[2:end]...)
    end
    ast
end

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
  while true
    #println("EVAL: $(printer.pr_str(ast,true))")
    if !isa(ast, Array) return eval_ast(ast, env) end

    # apply
    ast = macroexpand(ast, env)
    if !isa(ast, Array) return eval_ast(ast, env) end

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
    elseif :quote == ast[1]
        return ast[2]
    elseif :quasiquote == ast[1]
        ast = quasiquote(ast[2])
        # TCO loop
    elseif :defmacro! == ast[1]
        func = EVAL(ast[3], env)
        func.ismacro = true
        return env_set(env, ast[2], func)
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
                return EVAL(ast[3][3], Env(env, Any[ast[3][2]], Any[e]))
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
            (args...) -> EVAL(ast[3], Env(env, ast[2], Any[args...])),
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
env_set(repl_env, :eval, (ast) -> EVAL(ast, repl_env))
env_set(repl_env, symbol("*ARGV*"), ARGS[2:end])

# core.mal: defined using the language itself
REP("(def! *host-language* \"julia\")")
REP("(def! not (fn* (a) (if a false true)))")
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
REP("(def! *gensym-counter* (atom 0))")
REP("(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))")
REP("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))")


if length(ARGS) > 0
    REP("(load-file \"$(ARGS[1])\")")
    exit(0)
end

REP("(println (str \"Mal [\" *host-language* \"]\"))")
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
