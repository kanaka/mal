#!/usr/bin/env lua

local table = require('table')

local readline = require('readline')
local utils = require('utils')
local types = require('types')
local reader = require('reader')
local printer = require('printer')
local Env = require('env')
local core = require('core')
local List, Vector, HashMap = types.List, types.Vector, types.HashMap

-- read
function READ(str)
    return reader.read_str(str)
end

-- eval
function is_pair(x)
    return types._sequential_Q(x) and #x > 0
end

function quasiquote(ast)
    if not is_pair(ast) then
        return types.List:new({types.Symbol:new("quote"), ast})
    elseif types._symbol_Q(ast[1]) and ast[1].val == 'unquote' then
        return ast[2]
    elseif is_pair(ast[1]) and
           types._symbol_Q(ast[1][1]) and
           ast[1][1].val == 'splice-unquote' then
        return types.List:new({types.Symbol:new("concat"),
                               ast[1][2],
                               quasiquote(ast:slice(2))})
    else
        return types.List:new({types.Symbol:new("cons"),
                               quasiquote(ast[1]),
                               quasiquote(ast:slice(2))})
    end
end

function is_macro_call(ast, env)
    if types._list_Q(ast) and
       types._symbol_Q(ast[1]) and
       env:find(ast[1]) then
        local f = env:get(ast[1])
        return types._malfunc_Q(f) and f.ismacro
    end
end

function macroexpand(ast, env)
    while is_macro_call(ast, env) do
        local mac = env:get(ast[1])
        ast = mac.fn(unpack(ast:slice(2)))
    end
    return ast
end

function eval_ast(ast, env)
    if types._symbol_Q(ast) then
        return env:get(ast)
    elseif types._list_Q(ast) then
        return List:new(utils.map(function(x) return EVAL(x,env) end,ast))
    elseif types._vector_Q(ast) then
        return Vector:new(utils.map(function(x) return EVAL(x,env) end,ast))
    elseif types._hash_map_Q(ast) then
        local new_hm = {}
        for k,v in pairs(ast) do
            new_hm[EVAL(k, env)] = EVAL(v, env)
        end
        return HashMap:new(new_hm)
    else
        return ast
    end
end

function EVAL(ast, env)
  while true do
    --print("EVAL: "..printer._pr_str(ast,true))
    if not types._list_Q(ast) then return eval_ast(ast, env) end

    -- apply list
    ast = macroexpand(ast, env)
    if not types._list_Q(ast) then return eval_ast(ast, env) end

    local a0,a1,a2,a3 = ast[1], ast[2],ast[3],ast[4]
    if not a0 then return ast end
    local a0sym = types._symbol_Q(a0) and a0.val or ""
    if 'def!' == a0sym then
        return env:set(a1, EVAL(a2, env))
    elseif 'let*' == a0sym then
        local let_env = Env:new(env)
        for i = 1,#a1,2 do
            let_env:set(a1[i], EVAL(a1[i+1], let_env))
        end
        env = let_env
        ast = a2 -- TCO
    elseif 'quote' == a0sym then
        return a1
    elseif 'quasiquote' == a0sym then
        ast = quasiquote(a1) -- TCO
    elseif 'defmacro!' == a0sym then
        local mac = EVAL(a2, env)
        mac.ismacro = true
        return env:set(a1, mac)
    elseif 'macroexpand' == a0sym then
        return macroexpand(a1, env)
    elseif 'do' == a0sym then
        local el = eval_ast(ast:slice(2,#ast-1), env)
        ast = ast[#ast]  -- TCO
    elseif 'if' == a0sym then
        local cond = EVAL(a1, env)
        if cond == types.Nil or cond == false then
            if a3 then ast = a3 else return types.Nil end -- TCO
        else
            ast = a2 -- TCO
        end
    elseif 'fn*' == a0sym then
        return types.MalFunc:new(function(...)
            return EVAL(a2, Env:new(env, a1, arg))
        end, a2, env, a1)
    else
        local args = eval_ast(ast, env)
        local f = table.remove(args, 1)
        if types._malfunc_Q(f) then
            ast = f.ast
            env = Env:new(f.env, f.params, args) -- TCO
        else
            return f(unpack(args))
        end
    end
  end
end

-- print
function PRINT(exp)
    return printer._pr_str(exp, true)
end

-- repl
local repl_env = Env:new()
function rep(str)
    return PRINT(EVAL(READ(str),repl_env))
end

-- core.lua: defined using Lua
for k,v in pairs(core.ns) do
    repl_env:set(types.Symbol:new(k), v)
end
repl_env:set(types.Symbol:new('eval'),
             function(ast) return EVAL(ast, repl_env) end)
repl_env:set(types.Symbol:new('*ARGV*'), types.List:new(types.slice(arg,2)))

-- core.mal: defined using mal
rep("(def! not (fn* (a) (if a false true)))")
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")

if #arg > 0 and arg[1] == "--raw" then
    readline.raw = true
    table.remove(arg,1)
end

if #arg > 0 then
    rep("(load-file \""..arg[1].."\")")
    os.exit(0)
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
