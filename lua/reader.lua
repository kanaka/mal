local rex = require('rex_pcre')
local string = require('string')
local table = require('table')
local types = require('types')
local throw, Nil, Symbol, List = types.throw, types.Nil,
                                 types.Symbol, types.List

local M = {}

Reader = {}
function Reader:new(tokens)
    local newObj = {tokens = tokens, position = 1}
    self.__index = self
    return setmetatable(newObj, self)
end
function Reader:next()
    self.position = self.position + 1
    return self.tokens[self.position-1]
end
function Reader:peek()
    return self.tokens[self.position]
end

function M.tokenize(str)
    local results = {}
    local re_pos = 1
    local re = rex.new("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;[^\n]*|[^\\s\\[\\]{}('\"`,;)]*)", rex.flags().EXTENDED)
    while true do
        local s, e, t = re:exec(str, re_pos)
        if not s or s > e then break end
        re_pos = e + 1
        local val = string.sub(str,t[1],t[2])
        if string.sub(val,1,1) ~= ";" then
            table.insert(results, val)
        end
    end
    return results
end

function M.read_atom(rdr)
    local int_re = rex.new("^-?[0-9]+$")
    local float_re = rex.new("^-?[0-9][0-9.]*$")
    local token = rdr:next()
    if int_re:exec(token) then       return tonumber(token)
    elseif float_re:exec(token) then return tonumber(token)
    elseif string.sub(token,1,1) == '"' then
        local sval = string.sub(token,2,string.len(token)-1)
        sval = string.gsub(sval, '\\"', '"')
        sval = string.gsub(sval, '\\n', '\n')
        sval = string.gsub(sval, '\\\\', '\\')
        return sval
    elseif string.sub(token,1,1) == ':' then
        return "\177" .. string.sub(token,2)
    elseif token == "nil" then       return Nil
    elseif token == "true" then      return true
    elseif token == "false" then     return false
    else                             return Symbol:new(token)
    end
end

function M.read_sequence(rdr, start, last)
    local ast = {}
    local token = rdr:next()
    if token ~= start then throw("expected '"..start.."'") end

    token = rdr:peek()
    while token ~= last do
        if not token then throw("expected '"..last.."', got EOF") end
        table.insert(ast, M.read_form(rdr))
        token = rdr:peek()
    end
    rdr:next()
    return ast
end

function M.read_list(rdr)
    return types.List:new(M.read_sequence(rdr, '(', ')'))
end

function M.read_vector(rdr)
    return types.Vector:new(M.read_sequence(rdr, '[', ']'))
end

function M.read_hash_map(rdr)
    local seq = M.read_sequence(rdr, '{', '}')
    return types._assoc_BANG(types.HashMap:new(), unpack(seq))
end

function M.read_form(rdr)
    local token = rdr:peek()

    if "'" == token then
        rdr:next()
        return List:new({Symbol:new('quote'), M.read_form(rdr)})
    elseif '`' == token then
        rdr:next()
        return List:new({Symbol:new('quasiquote'), M.read_form(rdr)})
    elseif '~' == token then
        rdr:next()
        return List:new({Symbol:new('unquote'), M.read_form(rdr)})
    elseif '~@' == token then
        rdr:next()
        return List:new({Symbol:new('splice-unquote'), M.read_form(rdr)})
    elseif '^' == token then
        rdr:next()
        local meta = M.read_form(rdr)
        return List:new({Symbol:new('with-meta'), M.read_form(rdr), meta})
    elseif '@' == token then
        rdr:next()
        return List:new({Symbol:new('deref'), M.read_form(rdr)})

    elseif ')' == token then throw("unexpected ')'")
    elseif '(' == token then return M.read_list(rdr)
    elseif ']' == token then throw("unexpected ']'")
    elseif '[' == token then return M.read_vector(rdr)
    elseif '}' == token then throw("unexpected '}'")
    elseif '{' == token then return M.read_hash_map(rdr)
    else return M.read_atom(rdr)
    end
end

function M.read_str(str)
    local tokens = M.tokenize(str)
    if #tokens == 0 then error(nil) end
    return M.read_form(Reader:new(tokens))
end

return M
