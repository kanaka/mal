local core = {}
local Printer = require "printer"
local types = require "types"
local List = types.MalList
local Sym = types.Sym
local Vector = types.MalVector
local is_instanceOf = types.isinstanceof
local is_sequence = types.is_sequence
local Function = types.MalFunction
local Nil = types.Nil
local throw = types.throw
local Reader = require "reader"
local Atom = types.Atom
local Err = types.Err
local Hashmap = types.MalHashMap

core['pr-str'] = function (...)
  local res = ""
  local args = table.pack(...)
  for i,v in ipairs(args) do
    res = res .. Printer.stringfy_val(v, true) .. " "
  end

  return res:sub(1,#res-1)
end

core['str'] = function (...)
  local args = table.pack(...)
  local res = ""
  for i,v in ipairs(args) do
    res = res .. Printer.stringfy_val(v, false)
  end
  return res
end

core['prn'] = function (...)
  local res = ""
  local args = table.pack(...)
  for i,v in ipairs(args) do
    res = res .. Printer.stringfy_val(v, true) .. " "
  end

  print(res:sub(1,#res-1))
  return Nil
end

core['println'] = function (...)
  local args = table.pack(...)
  local res = ""
  for i,v in ipairs(args) do
    res = res .. Printer.stringfy_val(v, false) .. " "
  end
  print(res:sub(1,#res-1))
  return Nil
end

 


core['list'] = function (...)
  local args = table.pack(...)
  return List.new(args)
end

core['list?'] = function (v)
  if is_instanceOf(v, List) then
    return true
  else
    return false
  end
end

core['vector'] = function (...)
  local args = table.pack(...)
  return Vector.new(args)
end

core['vec'] = function (a)
  local nt = table.pack(table.unpack(a)) -- this is done for copying
  return Vector.new(nt)
end



core['empty?'] = function (v)
  if is_sequence(v) then
    return #v == 0
  end
  throw("'empty? expects a parameter to be sequence'")
end

core['count'] = function (v)
  if v == Nil then
    return 0
  end
  if is_sequence(v) then
    return #v
  end
  throw("'count expects parameter to be sequence or nil'")
end

core['<'] = function (a, b) return a < b end
core['>'] = function (a, b) return a > b end
core['<='] = function (a, b) return a <= b end
core['>='] = function (a, b) return a >= b end

core['+'] = function (a, b) return a + b end
core['-'] = function (a, b) return a - b end
core['*'] = function (a, b) return a * b end
core['/'] = function (a, b) return a / b end


core['='] = types.is_equal

core['read-string'] = Reader.read_str

core['slurp'] = function (filename)
  local f =  io.open(filename)
  if f == nil then
    throw(string.format("file '%s' cannot be opened", filename))
  end
  local res = f:read('a')
  f:close()
  return res
end

core['atom'] = function (v) return Atom.new(v) end
core['atom?'] = function (v) return types.is_atom(v) end
core['deref'] = function (v) return v.val end

core['reset!'] = function (v, malval)
  v.val = malval
  return v.val end
core['swap!'] =  function (v, f, ...)
  if not(is_instanceOf(f, Function) or type(f) == "function") then
    throw(string.format("second argument to swap! should be function"))
  end
  if is_instanceOf(f, Function) then
     f = f.fn
  end

  v.val = f(v.val, ...)
  return v.val
end

core['cons'] = function (first, second, ...)
  if ... ~= nil then throw("cons expect expects 2 args got: " .. 2 + #table.pack(...)) end
  if not(is_sequence(second)) then
    throw("second argument to cons should be Sequence")
  end
  local res = List.new({first, table.unpack(second)})

  return res

end

core['concat'] = function (...)
  local args = table.pack(...)
  local tmp = {}
  for i, v in ipairs(args) do
    if not(is_sequence(v))  then
      throw("argument to concat should be sequence at index:" .. i)
    end
    for ii, vv in ipairs(v) do
      table.insert(tmp, vv)
    end
  end
  local res = List.new(tmp)

  return res

end

core['nth'] = function (v, idx, ...)
  if ... ~= nil then throw("nth expect expects 2 args got: " .. 2 + #table.pack(...)) end
  if not(is_sequence(v)) then
    throw("first argument to nth should be Sequence")
  end
  if not(type(idx) == "number") then
    throw("second argument to nth should be number")
  end
  if idx > #v - 1 or idx < 0 then
    throw("index out of range")
  end
  return v[idx+1] or Nil
end

core['first'] = function (v, ...)
  if ... ~= nil then throw("first expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if not(is_sequence(v) or v == Nil) then
    throw("first argument to first should be Sequence or nil")
  end
  return v[1] or Nil
end

core['rest'] = function (v, ...)
  if ... ~= nil then throw("rest expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if not(is_sequence(v) or v == Nil) then
    throw("first argument to rest should be Sequence or nil")
  end
  if false and #v <= 1 then
    return Nil
  end 
  return List.new({table.unpack(v,2)}) 
end

core['throw'] = function (v, ...)
  if ... ~= nil then throw("throw expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then
    throw("")
  end

  throw(v) 
end

--fixme
core['map'] = function (f, seq, ...)  
  if ... ~= nil then 
    throw("map expect expects 1 args got: " .. 1 + #table.pack(...)) 
  end

  if not(is_instanceOf(f, Function) or type(f) == "function" ) then 
    throw("map expect first argument to be function") 
  end
  if not(is_sequence(seq)) then
    throw("map expect 2nd argument to be sequence") 
  end
  local constructor = Err.new
  if is_instanceOf(f, Function) then
    f = f.fn
  end
  local acc = {}
  for k,v  in ipairs(seq) do 
    --print("iterating " .. Printer.stringfy_val(k) .."--".. Printer.stringfy_val(v))
    table.insert(acc, f(v))
  end
  return List.new(acc)
  
end

core['apply'] = function (...)  
  local args = table.pack(...)
  if #args < 2  then 
    throw("apply expect at leasth 2 args got: " ..  #args) 
  end
  local f = args[1]
  if not(is_instanceOf(f, Function) or type(f) == "function" ) then 
    throw("apply expect first argument to be function") 
  end
  local last_arg = args[#args]
  if not(is_sequence(last_arg)) then
    throw("apply expect last argument to be sequence") 
  end
  if is_instanceOf(f, Function) then
    f = f.fn
  end
  for i=#args-1,2,-1 do
    table.insert(last_arg, 1, args[i])
  end
  return f(table.unpack(last_arg))
end



core['nil?'] = function (v, ...)
  if ... ~= nil then throw("nil? expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("nil? expect expects 1 args got: 0") end
  return v == Nil
end

core['true?'] = function (v, ...)
  if ... ~= nil then throw("true? expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("true? expect expects 1 args got: 0") end
  return v == true
end

core['false?'] = function (v, ...)
  if ... ~= nil then throw("false? expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("false? expect expects 1 args got: 0") end
  return v == false
end

core['symbol?'] = function (v, ...)
  if ... ~= nil then throw("symbol? expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("symbol? expect expects 1 args got: 0") end
  return is_instanceOf(v, Sym)
end

core['keyword?'] = function (v, ...)
  if ... ~= nil then throw("keyword? expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("keyword? expect expects 1 args got: 0") end
  return type(v) == "string" and "\u{029e}" == string.sub(v,1,2)

end

core['keyword'] = function (...)
  local args = table.pack(...)
  if #args ~= 1 then 
    throw("keyword expect expects 1 args got: " ..  #args)
  end
  local val = args[1]
  if core['keyword?'](val) then
    return val
  end
  if not( type(val) == "string") then
    throw("keyword expects string or keyword type")
  end
  return "\u{029e}" .. val
  
end

core['symbol'] = function (...)
  local args = table.pack(...)
  if #args ~= 1 then 
    throw("symbol expect expects 1 args got: " ..  #args)
  end
  local val = args[1]
  if core['symbol?'](val) then
    return val
  end
  if not( type(val) == "string") then
    throw("symbol expects string or symbol type")
  end
  return Sym.new(val)
  
end



core['sequential?'] = is_sequence

core['vector?'] = function (v, ...)
  if ... ~= nil then throw("vector? expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("vector? expect expects 1 args got: 0") end
  return is_instanceOf(v, Vector)
end

core['hash-map'] = Hashmap.new

core['keys'] = function (v, ...) 
  if ... ~= nil then throw("keys expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("keys expect expects 1 args got: 0") end
  if not(is_instanceOf(v, Hashmap)) then throw("keys expects its argument to be HashMap but got:" .. type(v) ) end

  local res = {}
  for k,_ in pairs(v) do
    table.insert(res, k)
  end
  return List.new(res)
 
end

core['vals'] = function (v, ...) 
  if ... ~= nil then throw("vals expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("vals expect expects 1 args got: 0") end
  if not(is_instanceOf(v, Hashmap)) then throw("vals expects its argument to be HashMap but got:" .. type(v) ) end

  local res = {}
  for _,val in pairs(v) do
    table.insert(res, val)
  end
  return List.new(res)
 
end


core['map?'] = function (v, ...) 
  if ... ~= nil then throw("map? expect expects 1 args got: " .. 1 + #table.pack(...)) end
  if v == nil then throw("map? expect expects 1 args got: 0") end
  return is_instanceOf(v, Hashmap)
end

core['hash-map'] = Hashmap.new

core['get'] = function (...)
  local args = table.pack(...)
  if #args ~= 2 then 
    throw("get expect expects 2 args got: " ..  #args)
  end
  local map = args[1]
  local key = args[2]
  if not(is_instanceOf(map, Hashmap) or map == Nil) then
    throw("get expects first arg to be hashmap or nil")
  end
   
  return map[key] and map[key] or Nil
end

core['contains?'] = function (...)
  local args = table.pack(...)
  if #args ~= 2 then 
    throw("contains? expect expects 1 args got: " ..  #args)
  end
  local map = args[1]
  local key = args[2]
  if not(is_instanceOf(map, Hashmap)) then
    throw("contains? expects first arg to be hashmap")
  end
     
  return map[key] and true or false
end


core['assoc'] = function (...)
  local args = table.pack(...)
  if #args % 2 ~= 1 then 
    throw("assoc expect expects odd number of args got: " ..  #args)
  end
  local map = table.remove(args,1)
  if not(is_instanceOf(map, Hashmap)) then
    throw("assoc expects first arg to be hashmap")
  end
  local res = Hashmap.new()
  for k,v in pairs(map) do
    res[k] = v
  end
  for i=1,#args,2 do
    res[args[i]] = args[i+1]
  end
   
  return res
end

core['dissoc'] = function (...)
  local args = table.pack(...)
  if #args  < 2 then 
    throw("assoc expect expects at least 2 args got: " ..  #args)
  end
  local map = table.remove(args,1) 
  if not(is_instanceOf(map, Hashmap)) then
    throw("assoc expects first arg to be HashMap")
  end
  local res = Hashmap.new()
  for k,v in pairs(map) do
    local keep = true
    for _, listval in ipairs(args) do
      if k == listval then 
        keep = false
      end
    end
    if keep then 
       res[k] = v
    end
  end
  return res
end




return core
