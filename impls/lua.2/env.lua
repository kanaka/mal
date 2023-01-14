local types = require "types"
local Printer = require "printer"
local throw = types.throw
local Sym = types.Sym
local List = types.MalList
local is_instanceOf = types.isinstanceof
local is_sequence = types.is_sequence

local Env = {}
Env.__index = Env

function Env.new(outer)
  local self = {}
  self.outer = outer
  setmetatable(self, Env)
  return self
end


function Env:bind(binds, exprs)
  if not(is_sequence(binds)) then throw("binds should be sequence") end
  if not(is_sequence(exprs)) then throw("exprs should be sequence") end

  if #binds ~= #exprs then
    throw("number of bindings and expressions should be equal")
  end
  for i, b in ipairs(binds) do
    if not(is_instanceOf(b, Sym)) then
      throw(string.format("%d/%d in the binds should be Symbol  ", i, #binds))
    end
    self[b.val] = exprs[i]
  end

end

function Env:set(key, val)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  self[key.val] = val
  return self
end

function Env:find(key)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  if self[key.val] then
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
    return env[key.val]
  end
  throw(string.format("%s not found in any environments.", Printer.stringfy_val(key)))
end

return Env
