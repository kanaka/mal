local types = require "types"
local Printer = require "printer"
local throw = types.throw
local Sym = types.Sym
local List = types.MalList
local is_instanceOf = types.isinstanceof

local Env = {}
Env.__index = Env

function Env.new(outer)
  local self = {}
  self.outer = outer
  setmetatable(self, Env)
  return self
end


function Env:bind(binds, exprs)
  if not(is_instanceOf(binds, List)) then throw("binds should be list") end
  if not(is_instanceOf(exprs, List)) then throw("exprs should be list") end

  if #binds ~= #exprs then
    throw("number of bindings and expressions should be equal")
  end
  for i, b in ipairs(binds) do
    if not(is_instanceOf(b, Sym)) then
      throw("%d/ in the binds should be Symbol  ",i, #binds)
    end
    print("binding " .. Printer.stringfy_val(b) .. " to " .. Printer.stringfy_val(exprs[i]))
    self[Printer.stringfy_val(b)] = exprs[i]
  end

end

function Env:set(key, val)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  self[Printer.stringfy_val(key)] = val
  return self
end

function Env:find(key)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  if self[Printer.stringfy_val(key)] then
    return self
  end
  if self.outer ~= nil then
   return self.outer:find(key)
  end
  return nil
end

function Env:get(key)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  local env = self:find(key)
  if env then
    return env[Printer.stringfy_val(key)]
  end
  throw(string.format("%s not found in any environments.", Printer.stringfy_val(key)))
end

return Env
