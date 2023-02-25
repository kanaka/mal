local Printer = {}

local Scanner = require "scanner"
local types = require "types"

local List = types.MalList
local Vector = types.MalVector
local Nil = types.Nil
local HashMap = types.MalHashMap
local Sym = types.Sym
local is_instanceOf = types.isinstanceof
local Err = types.Err
local Function = types.MalFunction
local Atom = types.Atom



function Printer.stringfy_val(val, readably)
  local res = ''
  if is_instanceOf(val, Vector) then
    res = res .. '['
    for i=1, #val do
      res = res .. Printer.stringfy_val(val[i],readably)
      if i ~= #val then
        res = res .. " "
      end
    end
    res = res .. ']'
  elseif is_instanceOf(val, List) then
    res = res .. '('
    for i=1, #val do
      res = res .. Printer.stringfy_val(val[i],readably)
      if i ~= #val then
        res = res .. " "
      end
    end
    res = res .. ')'
  elseif is_instanceOf(val, HashMap) then
    res = res .. '{'
    for i,v in pairs(val) do
      res = res .. Printer.stringfy_val(i, readably) .. " " .. Printer.stringfy_val(v,readably)
      res = res .. " "
    end
    if #res > 1 then
      res = string.sub(res, 1, #res-1) -- trim last space
    end
    res = res .. '}'

  elseif is_instanceOf(val, Sym) then
    return val.val
  elseif is_instanceOf(val, Err) then
    return "exc is: " .. Scanner.escape(val.val)
  elseif is_instanceOf(val, Function) then
    res = "(fn* "  .. Printer.stringfy_val(val.params) .. 
    "-->" .. Printer.stringfy_val(val.ast) ..")" .. "ismacro: " .. tostring(val.is_macro)
  elseif is_instanceOf(val, Atom) then
    res = "(atom " .. Printer.stringfy_val(val.val) .. ")"

  elseif type(val) == "string" then
    if "\u{29E}" == string.sub(val, 1, 2) then 
      return ":" .. string.sub(val, 3) end
    if readably then
      res = Scanner.unescape(val)
    else
      res = Scanner.escape(val)
    end
  elseif type(val) == "number" then
    res = tostring(val)
  elseif val == Nil then
    res = "nil"
  elseif type(val) == "boolean" then
    res = tostring(val)
  elseif type(val) == "function" then
    res = "#<function>"
  else
    error(string.format("Error: unknown type %s", val))
  end
  return res
end


return Printer
