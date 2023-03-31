#!/usr/bin/env lua
local Reader = require "reader"
local Printer = require "printer"
local types = require "types"
local Env = require "env"
local Sym = types.Sym
local is_instanceOf = types.isinstanceof
local Err = types.Err
local List = types.MalList
local throw = types.throw
local HashMap = types.MalHashMap
local Vector = types.MalVector
local Nil = types.Nil
local core = require "core"
local Function = types.MalFunction

function raw_read(prompt)
  io.write(prompt)
  local v = io.read()
  if v == nil then
    io.write('\n')
  end
  return v
end

function READ(str)
    return  Reader.read_str(str)
end

function starts_with(a, v)
return #a > 0 and  is_instanceOf(a[1],Sym) and 
        a[1].val == v
end


function quasiloop(a)
  local res = List.new({})
    for i=#a,1,-1 do
      local elt = a[i]
      if is_instanceOf(elt, List) and 
        starts_with(elt, "splice-unquote") then

      if #elt ~= 2 then throw("splice-unquote expected 1 argument bot got : " .. #elt) end

        res = List.new({Sym.new( "concat"), elt[2], res})
      else
        res = List.new({Sym.new( "cons"), quasiquote(elt), res})
      end
    end
    return res
end

function quasiquote(a)

      if is_instanceOf(a,List) then 
        if starts_with(a, "unquote") then
          if #a-1 ~= 1 then 
            throw("unquote expected 1 argument bot got : " .. #a) 
          end
          return a[2]
        else
          return quasiloop(a)
        end
      elseif is_instanceOf(a, Vector) then 
        local tmp = quasiloop(a)
        return List.new({Sym.new("vec"), tmp})
      elseif is_instanceOf(a,HashMap) or is_instanceOf(a,Sym) then
        return List.new({Sym.new('quote'), a})

      else
        return a

      end
end

function is_macro_call(ast, env)
    if is_instanceOf(ast, List) and #ast >= 1 and is_instanceOf(ast[1], Sym) then
    local status, first_env = pcall( function () return env:get(ast[1]) end)
    if not status then return false end
    if is_instanceOf(first_env, Function) and first_env.is_macro then
      return true
    else 
      return false
    end
  else
    return false
  end

end

function macro_expand(ast, env)
  while is_macro_call(ast, env) do
    local macro = env:get(ast[1])
    local f = macro.fn
    ast = f(table.unpack(ast, 2))
  end

  return ast
end

function EVAL(a, env)
  while true do
    a = macro_expand(a, env)
    if not(is_instanceOf(a, List)) then
       return eval_ast(a, env)
    end

    if #a == 0 then
      return a
    end
    local first_elem = a[1]
    local first_sym = is_instanceOf(first_elem, Sym) and first_elem.val or ""

    if first_sym == "def!" then
      if #a ~= 3 then 
        throw(string.format("def! expects 2 arguments got: %d", #a-1))
      end
      if not(is_instanceOf(a[2], Sym)) then
        throw("first argument to def! must be symbol")
      end
      local value = EVAL(a[3], env)
      env:set(a[2], value)
      return value

    elseif first_sym == "let*" then
      if #a ~= 3 then 
        throw(string.format("let* expects 2 arguments got: %d", #a-1))
      end

      local let_env = Env.new(env)
      if not(is_instanceOf(a[2], List) or is_instanceOf(a[2], Vector)) then
        throw("Second arg to let* should be list or vector")
      end
      if #a[2] % 2 ~= 0 then
        throw(string.format("Length ofSecond arg to let* should be even number got: %d", #a[2]))
      end

      for i=1,#a[2],2 do 
        if not(is_instanceOf(a[2][i], Sym)) then
          throw("Expected symbol in let*'s second argument")
        end
        local key = a[2][i]
        local value = EVAL(a[2][i+1],let_env)
        let_env:set(key,value)
      end 
      a = a[3]
      env = let_env 


    elseif first_sym == "do" then
      for i=2,#a-1 do 
        EVAL(a[i], env)
      end
      a = a[#a] --tco
      
    
    elseif first_sym == "if" then 
      if not (#a == 3 or #a == 4) then 
        throw("if expected 2 or 3 arguments but got '" .. #a-1 .. "'.") 
      end
      local cond = EVAL(a[2], env)
      if cond ~= false and cond ~= Nil then
        a = a[3]
      else
        if #a == 4 then 
          a = a[4] 
        else 
          return Nil
        end
      end
    elseif first_sym == "fn*" then 
      if (#a) ~= 3 then throw("fn* expected 2 arguments but got '" .. #a-1 .. "'.") end
      if false then throw("second parameter to fn* should have length 2 but got '" .. #a[2] .. "'.") end
     return Function.new(function (...) 
        local closed_over_env = Env.new(env)
        local exprs = List.new(table.pack(...))
        local binds = a[2]
        closed_over_env:bind(binds, exprs)
      
        return EVAL(a[3], closed_over_env) 
      end, a[3], env, a[2], false)

    elseif first_sym == "quote" then
      if #a-1 ~= 1 then throw("quote expects 1 argument got '" .. #a-1 .. "'.") end
      return a[2]
    elseif first_sym == "quasiquote" then
      if #a-1 ~= 1 then throw("quote expects 1 argument got '" .. #a-1 .. "'.") end
      a = quasiquote(a[2])
    elseif first_sym == "defmacro!" then
      if #a ~= 3 then 
        throw(string.format("defmacro! expects 2 arguments got: %d", #a-1))
      end
      if not(is_instanceOf(a[2], Sym)) then
        throw("first argument to defmacro! must be symbol")
      end
      local value = EVAL(a[3], env)
      if not(is_instanceOf(value, Function)) then
        throw("second argument to defmacro must be function")
      end
      value.is_macro = true 
      env:set(a[2], value)
      return value


    elseif first_sym == "macroexpand" then
      if (#a) ~= 2 then throw("macroexpand expected 1 arguments but got '" .. #a-1 .. "'.") end
      return macro_expand(a[2],env)
    else 
  
      local args = eval_ast(a, env) 
      local f = table.remove(args,1)
      if types.is_malfunc(f) then
        a = f.ast
        env = Env.new(f.env)
        env:bind(f.params, args)
        
      else
        if type(f) ~= "function" then
          throw("First elem should be function or special form got :'" .. type(f) .. "'.")
        end 
        return f(table.unpack(args)) --fixme: varargs?
      end
    end

end
end


function eval_ast(ast, env)
  if is_instanceOf(ast, List) then
    local l = List.new()
    for i=1,#ast do
      table.insert(l, EVAL(ast[i], env))
    end
    return l
  elseif is_instanceOf(ast, Vector) then
    local v = Vector.new()
    for i=1, #ast do
      table.insert(v, EVAL(ast[i], env))
    end
    return v
  elseif is_instanceOf(ast, HashMap) then
    local map = HashMap.new()
    for k,v in pairs(ast) do
      map[EVAL(k, env)] = EVAL(v, env)
    end
    return map
  elseif is_instanceOf(ast, Sym) then
    if string.byte(ast.val, 1, 1) == 202  and 
       string.byte(ast.val, 2, 2) == 158  then  -- this magic numbers come from \u{29E}
      return Sym.new(":" .. string.sub(ast.val, 3, #ast.val))
    end
    return env:get(ast)
  
  else
    return ast
  end
end


function PRINT(a)
  return Printer.stringfy_val(a, true)
end


local repl_env = Env.new(nil)

for k,v in pairs(core) do
  repl_env:set(Sym.new(k),v)
end

repl_env:set(Sym.new('eval'), function (ast)
  return EVAL(ast, repl_env)
end)


function rep(str)
  return PRINT(EVAL(READ(str), repl_env))
end



function main()
    rep("(def! not (fn* (a) (if a false true)))")
    rep("(def! load-file (fn* (f) (eval (read-string (str \"(do  \"(slurp f) \"\nnil)\")))))")
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
    repl_env:set(Sym.new("*ARGV*"), List.new(table.pack(table.unpack(arg,2))))
    if #arg > 0 then 
      local file_to_run = table.remove(arg,1)
      rep("(load-file \"" .. file_to_run .. "\")") 
      os.exit(0)
    end

    local line = ''
    while true do
      line = raw_read('user> ')
      if line == nil then
        break
      end
      local status, err = pcall(function () print(rep(line)) end)
      if not status then
        if is_instanceOf(err, Err) then
          err = Printer.stringfy_val(err)
        end
        print(err)
        print(debug.traceback())
      end
    end
end

main()
