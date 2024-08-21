local string = require('string')
local table = require('table')
local types = require('types')
local utils = require('utils')

local M = {}

function M._pr_str(obj, print_readably)
    if utils.instanceOf(obj, types.Symbol) then
        return obj.val
    elseif types._list_Q(obj) then
        return "(" .. M._pr_seq(obj, print_readably, " ") .. ")"
    elseif types._vector_Q(obj) then
        return "[" .. M._pr_seq(obj, print_readably, " ") .. "]"
    elseif types._hash_map_Q(obj) then
        local res = {}
        for k,v in pairs(obj) do
            res[#res+1] = M._pr_str(k, print_readably)
            res[#res+1] = M._pr_str(v, print_readably)
        end
        return "{".. table.concat(res, " ").."}"
    elseif types._keyword_Q(obj) then
         return ':' .. types._lua_string_from_keyword(obj)
    elseif types._string_Q(obj) then
            if print_readably then
                local sval = obj:gsub('\\', '\\\\')
                sval = sval:gsub('"', '\\"')
                sval = sval:gsub('\n', '\\n')
                return '"' .. sval .. '"'
            else
                return obj
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

function M._pr_seq(obj, print_readably, separator)
    return table.concat(
        utils.map(function(e) return M._pr_str(e,print_readably) end,
                  obj),
        separator)
end

return M
