types = require "types"
Reader = require "reader"
throw = types.throw
Sym = types.Sym
is_instanceOf = types.isinstanceof

local M = {}
M.Env = {}
M.Env.__index = M.Env

function M.Env.new(outer)
  local self = {}
  self.outer = outer
  setmetatable(self, M.Env)
  return self
end


function M.Env:set(key, val)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  self[Reader.stringfy_val(key)] = val
  return self
end

function M.Env:find(key)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  if self[Reader.stringfy_val(key)] then
    return self
  end
  if self.outer ~= nil then
   return self.outer:find(key)
  end
  return nil
  
end

function M.Env:get(key)
  assert(is_instanceOf(key, Sym), "key should be symbol")
  local env = self:find(key)
  if env then
    return env[Reader.stringfy_val(key)]
  end
  throw(string.format("%s not found in any environments.", Reader.stringfy_val(key)))
  
end

return M.Env
