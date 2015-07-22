local utils = require('utils')

local M = {}

-- type functions

function M._sequential_Q(obj)
    return M._list_Q(obj) or M._vector_Q(obj)
end

function M._equal_Q(a,b)
    if M._symbol_Q(a) and M._symbol_Q(b) then
        return a.val == b.val
    elseif M._sequential_Q(a) and M._sequential_Q(b) then
        if #a ~= #b then return false end
        for i, v in ipairs(a) do
            if not M._equal_Q(v,b[i]) then return false end
        end
        return true
    else
        return a == b
    end
end

function M.copy(obj)
    if type(obj) == "function" then
        return M.FunctionRef:new(obj)
    end
    if type(obj) ~= "table" then return obj end

    -- copy object data
    local new_obj = {}
    for k,v in pairs(obj) do
        new_obj[k] = v
    end

    -- copy metatable and link to original
    local old_mt = getmetatable(obj)
    if old_mt ~= nil then
        local new_mt = {}
        for k,v in pairs(old_mt) do
            new_mt[k] = v
        end
        setmetatable(new_mt, old_mt)
        setmetatable(new_obj, new_mt)
    end

    return new_obj
end

function M.slice(lst, start, last)
    if last == nil then last = #lst end
    local new_lst = {}
    if start <= last then
        for i = start, last do
            new_lst[#new_lst+1] = lst[i]
        end
    end
    return new_lst
end

-- Error/exceptions

M.MalException = {}
function M.MalException:new(val)
    local newObj = {val = val}
    self.__index = self
    return setmetatable(newObj, self)
end
function M._malexception_Q(obj)
    return utils.instanceOf(obj, M.MalException)
end

function M.throw(val)
    error(M.MalException:new(val))
end

-- Nil

local NilType = {}
function NilType:new(val)
    local newObj = {}
    self.__index = self
    return setmetatable(newObj, self)
end
M.Nil = NilType:new()
function M._nil_Q(obj)
    return obj == Nil
end

-- Strings
function M._string_Q(obj)
    return type(obj) == "string"
end

-- Symbols

M.Symbol = {}
function M.Symbol:new(val)
    local newObj = {val = val}
    self.__index = self
    return setmetatable(newObj, self)
end
function M._symbol_Q(obj)
    return utils.instanceOf(obj, M.Symbol)
end

-- Keywords
function M._keyword_Q(obj)
    return M._string_Q(obj) and "\177" == string.sub(obj,1,1)
end


-- Lists

M.List = {}
function M.List:new(lst)
    local newObj = lst and lst or {}
    self.__index = self
    return setmetatable(newObj, self)
end
function M._list_Q(obj)
    return utils.instanceOf(obj, M.List)
end
function M.List:slice(start,last)
    return M.List:new(M.slice(self,start,last))
end

-- Vectors

M.Vector = {}
function M.Vector:new(lst)
    local newObj = lst and lst or {}
    self.__index = self
    return setmetatable(newObj, self)
end
function M._vector_Q(obj)
    return utils.instanceOf(obj, M.Vector)
end
function M.Vector:slice(start,last)
    return M.Vector:new(M.slice(self,start,last))
end

-- Hash Maps
--
M.HashMap = {}
function M.HashMap:new(val)
    local newObj = val and val or {}
    self.__index = self
    return setmetatable(newObj, self)
end
function M.hash_map(...)
    return M._assoc_BANG(M.HashMap:new(), unpack(arg))
end
function M._hash_map_Q(obj)
    return utils.instanceOf(obj, M.HashMap)
end
function M._assoc_BANG(hm, ...)
    for i = 1, #arg, 2 do
        hm[arg[i]] = arg[i+1]
    end
    return hm
end
function M._dissoc_BANG(hm, ...)
    for i = 1, #arg do
        hm[arg[i]] = nil
    end
    return hm
end

-- Functions

M.MalFunc = {}
function M.MalFunc:new(fn, ast, env, params)
    local newObj = {fn = fn, ast = ast, env = env,
                    params = params, ismacro = false}
    self.__index = self
    return setmetatable(newObj, self)
end
function M._malfunc_Q(obj)
    return utils.instanceOf(obj, M.MalFunc)
end

-- Atoms

M.Atom = {}
function M.Atom:new(val)
    local newObj = {val = val}
    self.__index = self
    return setmetatable(newObj, self)
end
function M._atom_Q(obj)
    return utils.instanceOf(obj, M.Atom)
end

-- FunctionRefs

M.FunctionRef = {}
function M.FunctionRef:new(fn)
    local newObj = {fn = fn}
    return setmetatable(newObj, self)
end
function M._functionref_Q(obj)
    return utils.instanceOf(obj, M.FunctionRef)
end
function M.FunctionRef:__call(...)
    return self.fn(...)
end

return M
