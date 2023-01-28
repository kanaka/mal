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
  local data = {}
  local self = {}
  self.outer = outer
  self.data = data
  setmetatable(self, Env)
  return self
end

function Env:bind(binds, exprs)
  if not(is_sequence(binds)) then throw("binds should be sequence") end
  if not(is_sequence(exprs)) then throw("exprs should be sequence") end


  
  for i, b in ipairs(binds) do
    if not(is_instanceOf(b, Sym)) then
      throw(string.format("%d/%d in the binds should be Symbol  ", i, #binds))
    end
    if b.val ~= '&' then
      self.data[b.val] = exprs[i]
      --print(b.val .. ":" .. Printer.stringfy_val(exprs[i]) )
    else
      if i == #binds or not(is_instanceOf(binds[i+1],Sym)) then 
        throw("Symbol '&' should be followed by an another symbol")
      end
      self.data[binds[i+1].val] = List.new(table.pack(table.unpack(exprs,i)))
      break
    end
  end

end

function Env:set(key, val)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  self.data[key.val] = val
  return self
end

function Env:find(key)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  if self.data[key.val] ~= nil then
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
    return env.data[key.val]
  end
  throw(string.format("%s not found in any environments.", Printer.stringfy_val(key)))
end

return Env
