#!/usr/bin/env lua

local table = require('table')

package.path = '../lua/?.lua;' .. package.path
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
function starts_with(ast, sym)
    return 0 < #ast and types._symbol_Q(ast[1]) and ast[1].val == sym
end

function quasiquote_loop(ast)
   local acc = types.List:new({})
   for i = #ast,1,-1 do
      local elt = ast[i]
      if types._list_Q(elt) and starts_with(elt, "splice-unquote") then
         acc = types.List:new({types.Symbol:new("concat"), elt[2], acc})
      else
         acc = types.List:new({types.Symbol:new("cons"), quasiquote(elt), acc})
      end
   end
   return acc
end

function quasiquote(ast)
    if types._list_Q(ast) then
        if starts_with(ast, "unquote") then
            return ast[2]
        else
            return quasiquote_loop(ast)
        end
    elseif types._vector_Q(ast) then
        return types.List:new({types.Symbol:new("vec"), quasiquote_loop(ast)})
    elseif types._symbol_Q(ast) or types._hash_map_Q(ast) then
        return types.List:new({types.Symbol:new("quote"), ast})
    else
        return ast
    end
end

function EVAL(ast, env)
  while true do

    local dbgeval = env:get("DEBUG-EVAL")
    if dbgeval ~= nil and dbgeval ~= types.Nil and dbgeval ~= false then
        print("EVAL: " .. printer._pr_str(ast, true))
        env:debug()
    end

    if types._symbol_Q(ast) then
        local result = env:get(ast.val)
        if result == nil then
            types.throw("'" .. ast.val .. "' not found")
        end
        return result
    elseif types._vector_Q(ast) then
        return Vector:new(utils.map(function(x) return EVAL(x,env) end,ast))
    elseif types._hash_map_Q(ast) then
        local new_hm = {}
        for k,v in pairs(ast) do
            new_hm[k] = EVAL(v, env)
        end
        return HashMap:new(new_hm)
    elseif not types._list_Q(ast) or #ast == 0 then
        return ast
    end

    -- apply list

    local a0,a1,a2,a3 = ast[1], ast[2],ast[3],ast[4]
    local a0sym = types._symbol_Q(a0) and a0.val or ""
    if 'def!' == a0sym then
        return env:set(a1.val, EVAL(a2, env))
    elseif 'let*' == a0sym then
        local let_env = Env:new(env)
        for i = 1,#a1,2 do
            let_env:set(a1[i].val, EVAL(a1[i+1], let_env))
        end
        env = let_env
        ast = a2 -- TCO
    elseif 'quote' == a0sym then
        return a1
    elseif 'quasiquote' == a0sym then
        ast = quasiquote(a1) -- TCO
    elseif 'defmacro!' == a0sym then
        local mac = types.copy(EVAL(a2, env))
        mac.ismacro = true
        return env:set(a1.val, mac)
    elseif 'try*' == a0sym then
      if a2 == nil or a2[1].val ~= 'catch*' then
        ast = a1 -- TCO
      else
        local exc, result = nil, nil
        xpcall(function()
            result = EVAL(a1, env)
        end, function(err)
            exc = err
        end)
        if exc == nil then
            return result
        else
            if types._malexception_Q(exc) then
                exc = exc.val
            end
            ast, env = a2[3], Env:new(env, {a2[2]}, {exc}) -- TCO
        end
      end
    elseif 'do' == a0sym then
        utils.map(function(x) return EVAL(x, env) end, types.slice(ast, 2, #ast - 1))
        ast = ast[#ast]  -- TCO
    elseif 'if' == a0sym then
        local cond = EVAL(a1, env)
        if cond == types.Nil or cond == false then
            if #ast > 3 then ast = a3 else return types.Nil end -- TCO
        else
            ast = a2 -- TCO
        end
    elseif 'fn*' == a0sym then
        return types.MalFunc:new(function(...)
            return EVAL(a2, Env:new(env, a1, table.pack(...)))
        end, a2, env, a1)
    else
      local f = EVAL(a0, env)
      local args = types.slice(ast, 2)
      if types._macro_Q(f) then
        ast = f.fn(table.unpack(args)) -- TCO
      else
        args = utils.map(function(x) return EVAL(x,env) end, args)
        if types._malfunc_Q(f) then
            ast = f.ast
            env = Env:new(f.env, f.params, args) -- TCO
        else
            return f(table.unpack(args))
        end
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
    repl_env:set(k, v)
end
repl_env:set('eval',
             function(ast) return EVAL(ast, repl_env) end)
repl_env:set('*ARGV*', types.List:new(types.slice(arg,2)))

-- core.mal: defined using mal
rep("(def! *host-language* \"lua\")")
rep("(def! not (fn* (a) (if a false true)))")
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

function print_exception(exc)
    if exc then
        if types._malexception_Q(exc) then
            exc = printer._pr_str(exc.val, true)
        end
        print("Error: " .. exc)
        print(debug.traceback())
    end
end

if #arg > 0 and arg[1] == "--raw" then
    readline.raw = true
    table.remove(arg,1)
end

if #arg > 0 then
    xpcall(function() rep("(load-file \""..arg[1].."\")") end,
           print_exception)
    os.exit(0)
end

rep("(println (str \"Mal [\" *host-language* \"]\"))")
while true do
    line = readline.readline("user> ")
    if not line then break end
    xpcall(function() print(rep(line)) end,
           print_exception)
end
