local core = {}
local Printer = require "printer"
local types = require "types"
local List = types.MalList
local Sym = types.Sym
local Vector = types.MalVector
local is_instanceOf = types.isinstanceof
local is_sequence = types.is_sequence
local Nil = types.Nil
local throw = types.throw

core[Sym.new('pr-str')] = function (...) 
  local res = ""
  local args = table.pack(...)
  for i,v in ipairs(args) do 
    res = res .. Printer.stringfy_val(v, true) .. " "
  end

  return res:sub(1,#res-1) 
end

core[Sym.new('str')] = function (...)
  local args = table.pack(...)
  local res = ""
  for i,v in ipairs(args) do 
    res = res .. Printer.stringfy_val(v, false)
  end
  return res
end

 core[Sym.new('prn')] = function (...) 
  local res = ""
  local args = table.pack(...)
  for i,v in ipairs(args) do 
    res = res .. Printer.stringfy_val(v, true) .. " "
  end

  print(res:sub(1,#res-1))
  return Nil
end

core[Sym.new('println')] = function (...)
  local args = table.pack(...)
  local res = ""
  for i,v in ipairs(args) do 
    res = res .. Printer.stringfy_val(v, false) .. " "
  end
  print(res:sub(1,#res-1))
  return Nil
end

 


core[Sym.new('list')] = function (...) 
  local args = table.pack(...) 
  return List.new(args) 
end

core[Sym.new('list?')] = function (v) 
  if is_instanceOf(v, List) then
    return true
  else
    return false
  end
end

core[Sym.new('empty?')] = function (v) 
  if is_sequence(v) then
    return #v == 0
  end
  throw("'empty? expects a parameter to be sequence'")
end

core[Sym.new('count')] = function (v)
  if v == Nil then 
    return 0
  end
  if is_sequence(v) then
    return #v
  end
  throw("'count expects parameter to be sequence or nil'")
end

core[Sym.new('<')] = function (a, b) return a < b end
core[Sym.new('>')] = function (a, b) return a > b end
core[Sym.new('<=')] = function (a, b) return a <= b end
core[Sym.new('>=')] = function (a, b) return a >= b end

core[Sym.new('+')] = function (a, b) return a + b end
core[Sym.new('-')] = function (a, b) return a - b end
core[Sym.new('*')] = function (a, b) return a * b end
core[Sym.new('/')] = function (a, b) return a / b end

  
core[Sym.new('=')] = types.is_equal


return core
