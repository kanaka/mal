local rex = require('rex_pcre')
local string = require('string')
local table = require('table')
local utils = require('utils')
local types = require('types')

local Env = {}

function Env:new(outer, binds, exprs)
    local data = {}
    local newObj = {outer = outer, data = data}
    self.__index = self
    if binds then
        for i, b in ipairs(binds) do
            if binds[i].val == '&' then
                local new_exprs = types.List:new()
                for j = i, #exprs do
                    table.insert(new_exprs, exprs[j])
                end
                table.remove(exprs, 1)
                data[binds[i+1].val] = new_exprs
                break
            end
            data[binds[i].val] = exprs[i]
        end
    end
    return setmetatable(newObj, self)
end
function Env:find(sym)
    if self.data[sym.val] ~= nil then
        return self
    else
        if self.outer ~= nil then
            return self.outer:find(sym)
        else
            return nil
        end
    end
end
function Env:set(sym,val)
    self.data[sym.val] = val
    return val
end
function Env:get(sym)
    local env = self:find(sym)
    if env then
        return env.data[sym.val]
    else
        types.throw("'"..sym.val.."' not found")
    end
end

return Env
