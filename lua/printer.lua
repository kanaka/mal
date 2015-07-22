local string = require('string')
local table = require('table')
local types = require('types')
local utils = require('utils')

local M = {}

function M._pr_str(obj, print_readably)
    local _r = print_readably
    if utils.instanceOf(obj, types.Symbol) then
        return obj.val
    elseif types._list_Q(obj) then
        return "(" .. table.concat(utils.map(function(e)
            return M._pr_str(e,_r) end, obj), " ") .. ")"
    elseif types._vector_Q(obj) then
        return "[" .. table.concat(utils.map(function(e)
            return M._pr_str(e,_r) end, obj), " ") .. "]"
    elseif types._hash_map_Q(obj) then
        local res = {}
        for k,v in pairs(obj) do
            res[#res+1] = M._pr_str(k, _r)
            res[#res+1] = M._pr_str(v, _r)
        end
        return "{".. table.concat(res, " ").."}"
    elseif type(obj) == 'string' then
        if string.sub(obj,1,1) == "\177" then
            return ':' .. string.sub(obj,2)
        else
            if _r then
                local sval = obj:gsub('\\', '\\\\')
                sval = sval:gsub('"', '\\"')
                sval = sval:gsub('\n', '\\n')
                return '"' .. sval .. '"'
            else
                return obj
            end
        end
    elseif obj == types.Nil then
        return "nil"
    elseif obj == true then
        return "true"
    elseif obj == false then
        return "false"
    elseif types._malfunc_Q(obj) then
        return "(fn* "..M._pr_str(obj.params).." "..M._pr_str(obj.ast)..")"
    elseif types._atom_Q(obj) then
        return "(atom "..M._pr_str(obj.val)..")"
    elseif type(obj) == 'function' or types._functionref_Q(obj) then
        return "#<function>"
    else
        return string.format("%s", obj)
    end
end

return M
