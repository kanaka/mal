Reader = require "reader"
types = require "types"
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
    elseif #a == 3 then
      new_list = eval_ast(a, env)
      return new_list[1](new_list[2],new_list[3])
    else 
      throw("'" .. Reader.stringfy_val(a[1]) .. "' should be called with 2 elements")
    end
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
    for i,v in pairs(ast) do
      map[EVAL(i, env)] = EVAL(v, env)
    end
    return map
  elseif is_instanceOf(ast, Sym) then
    if env[ast.val] == nil then
      types.throw(string.format("Value : '%s' does not exist", ast.val))
    else
      return env[ast.val]
    end
  elseif type(ast) == "number" or type(ast) == "string" or type(ast) == "function" then
    return ast
  else
    throw("dont know how to eval")
  end
end


function PRINT(a)
  print(Reader.stringfy_val(a, true))
end


local repl_env = { 
['+'] = function (a,b) return a+b end,
['-'] = function (a,b) return a-b end,
['*'] = function (a,b) return a*b end,
['/'] = function (a,b) return a/b end,
['a'] = types.MalList.new({1,2,3}),

['n'] = 3,
}

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
