Reader = require "reader"
types = require "types"
Env = require "env"
local Sym = types.Sym
local is_instanceOf = types.isinstanceof
local Err = types.Err
local List = types.MalList
local throw = types.throw
local HashMap = types.MalHashMap
local Vector = types.MalVector

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

function EVAL(a, env)
  if is_instanceOf(a, List) then
    if #a == 0 then
      return a
    end
    local first_elem = a[1]

    if is_instanceOf(first_elem, Sym) and first_elem.val == "def!" then
      if #a ~= 3 then 
        throw(string.format("def! expects 2 arguments got: %d", #a-1))
      end
      if not(is_instanceOf(a[2], Sym)) then
        throw("first argument to def! must be symbol")
      end
      local value = EVAL(a[3], env)
      env:set(a[2], value)
      return value
    end

    if is_instanceOf(first_elem, Sym) and first_elem.val == "let*" then
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
      return EVAL(a[3], let_env)

    end

    local new_list = eval_ast(a, env) 
    if type(new_list[1]) ~= "function" then
      throw("First elem should be function or special form got :'" .. type(new_list[1]) .. "'.")
    end

    if #a ~= 3 then 
      throw("currently all builtins expects 2 arguments")
    end
    return new_list[1](new_list[2],new_list[3]) --fixme: varargs?

  else
     return eval_ast(a, env)
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
  print(Reader.stringfy_val(a, true))
end


local repl_env = Env.new(nil)

repl_env = repl_env:set(Sym.new('+'), function (a,b) return a+b end)
repl_env:set(Sym.new('-'), function (a,b) return a-b end)
repl_env:set(Sym.new('*'), function (a,b) return a*b end)
repl_env:set(Sym.new('/'), function (a,b) return a/b end)


function rep(str)
  return PRINT(EVAL(READ(str), repl_env))
end


function main()
    local line = ''
    while true do
      line = raw_read('user> ')
      if line == nil then
        break
      end
      status, err = pcall(function () print(rep(line)) end)
      if not status then
        if is_instanceOf(err, Err) then
          err = Reader.stringfy_val(err)
        end
        print(err)
        print(debug.traceback())
      end
    end
end

main()
