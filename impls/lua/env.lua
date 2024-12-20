local table = require('table')
local types = require('types')
local printer = require('printer')

local Env = {}

function Env:new(outer, binds, exprs)
    -- binds is a MAL sequence of MAL symbols
    -- exprs is an LUA table of MAL forms
    local data = {}
    local newObj = {outer = outer, data = data}
    self.__index = self
    if binds then
        for i, b in ipairs(binds) do
            if binds[i].val == '&' then
                data[binds[i+1].val] = types.List.slice(exprs, i)
                break
            end
            data[binds[i].val] = exprs[i]
        end
    end
    return setmetatable(newObj, self)
end

function Env:get(sym)
    -- sym is an LUA string
    -- returns nil if the key is not found
    local env = self
    local result
    while true do
        result = env.data[sym]
        if result ~= nil then return result end
        env = env.outer
        if env == nil then return nil end
    end
end

function Env:set(sym,val)
    -- sym is an LUA string
    self.data[sym] = val
    return val
end

function Env:debug()
    local env = self
    while env.outer ~=nil do
        line = '  ENV:'
        for k, v in pairs(env.data) do
            line = line .. ' ' .. k .. '=' .. printer._pr_str(v)
        end
        print(line)
        env = env.outer
    end
end

return Env
