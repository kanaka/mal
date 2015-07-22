local utils = require('utils')
local types = require('types')
local reader = require('reader')
local printer = require('printer')
local readline = require('readline')

local Nil, List, _pr_str = types.Nil, types.List, printer._pr_str

local M = {}

-- string functions

function pr_str(...)
    return table.concat(
        utils.map(function(e) return _pr_str(e, true) end, arg), " ")
end

function str(...)
    return table.concat(
        utils.map(function(e) return _pr_str(e, false) end, arg), "")
end

function prn(...)
    print(table.concat(
        utils.map(function(e) return _pr_str(e, true) end, arg), " "))
    io.flush()
    return Nil
end

function println(...)
    print(table.concat(
        utils.map(function(e) return _pr_str(e, false) end, arg), " "))
    io.flush()
    return Nil
end

function slurp(file)
    local lines = {}
    for line in io.lines(file) do
        lines[#lines+1] = line
    end
    return table.concat(lines, "\n") .. "\n"
end

function do_readline(prompt)
    local line = readline.readline(prompt)
    if line == nil then
        return Nil
    else
        return line
    end
end

-- hash map functions

function assoc(hm, ...)
    return types._assoc_BANG(types.copy(hm), unpack(arg))
end

function dissoc(hm, ...)
    return types._dissoc_BANG(types.copy(hm), unpack(arg))
end

function get(hm, key)
    local res = hm[key]
    if res == nil then return Nil end
    return res
end

function keys(hm)
    local res = {}
    for k,v in pairs(hm) do
        res[#res+1] = k
    end
    return List:new(res)
end

function vals(hm)
    local res = {}
    for k,v in pairs(hm) do
        res[#res+1] = v
    end
    return List:new(res)
end

-- sequential functions

function cons(a,lst)
    local new_lst = lst:slice(1)
    table.insert(new_lst, 1, a)
    return List:new(new_lst)
end

function concat(...)
    local new_lst = {}
    for i = 1, #arg do
        for j = 1, #arg[i] do
            table.insert(new_lst, arg[i][j])
        end
    end
    return List:new(new_lst)
end

function nth(seq, idx)
    if idx+1 <= #seq then
        return seq[idx+1]
    else
        types.throw("nth: index out of range")
    end
end

function first(a)
    if #a == 0 then
        return Nil
    else
        return a[1]
    end
end

function apply(f, ...)
    if types._malfunc_Q(f) then
        f = f.fn
    end
    local args = concat(types.slice(arg, 1, #arg-1),
                        arg[#arg])
    return f(unpack(args))
end

function map(f, lst)
    if types._malfunc_Q(f) then
        f = f.fn
    end
    return List:new(utils.map(f, lst))
end

-- metadata functions

function meta(obj)
    local m = getmetatable(obj)
    if m == nil or m.meta == nil then return Nil end
    return m.meta
end

function with_meta(obj, meta)
    local new_obj = types.copy(obj)
    getmetatable(new_obj).meta = meta
    return new_obj
end

-- atom functions

function swap_BANG(atm,f,...)
    if types._malfunc_Q(f) then
        f = f.fn
    end
    local args = List:new(arg)
    table.insert(args, 1, atm.val)
    atm.val = f(unpack(args))
    return atm.val
end

local function conj(obj, ...)
    local new_obj = types.copy(obj)
    if types._list_Q(new_obj) then
        for i, v in ipairs(arg) do
            table.insert(new_obj, 1, v)
        end
    else
        for i, v in ipairs(arg) do
            table.insert(new_obj, v)
        end
    end
    return new_obj
end

M.ns = {
    ['='] =  types._equal_Q,
    throw = types.throw,

    ['nil?'] =  function(a) return a==Nil end,
    ['true?'] =  function(a) return a==true end,
    ['false?'] =  function(a) return a==false end,
    symbol = function(a) return types.Symbol:new(a) end,
    ['symbol?'] = function(a) return types._symbol_Q(a) end,
    keyword = function(a) return "\177"..a end,
    ['keyword?'] = function(a) return types._keyword_Q(a) end,

    ['pr-str'] = pr_str,
    str = str,
    prn = prn,
    println = println,
    ['read-string'] = reader.read_str,
    readline = do_readline,
    slurp = slurp,

    ['<'] =  function(a,b) return a<b end,
    ['<='] = function(a,b) return a<=b end,
    ['>'] =  function(a,b) return a>b end,
    ['>='] = function(a,b) return a>=b end,
    ['+'] =  function(a,b) return a+b end,
    ['-'] =  function(a,b) return a-b end,
    ['*'] =  function(a,b) return a*b end,
    ['/'] =  function(a,b) return math.floor(a/b) end,
    -- TODO: get actual milliseconds
    ['time-ms'] = function() return os.time() * 1000 end,

    list = function(...) return List:new(arg) end,
    ['list?'] = function(a) return types._list_Q(a) end,
    vector = function(...) return types.Vector:new(arg) end,
    ['vector?'] = types._vector_Q,
    ['hash-map'] = types.hash_map,
    ['map?'] = types._hash_map_Q,
    assoc = assoc,
    dissoc = dissoc,
    get = get,
    ['contains?'] = function(a,b) return a[b] ~= nil end,
    keys = keys,
    vals = vals,

    ['sequential?'] = types._sequential_Q,
    cons = cons,
    concat = concat,
    nth = nth,
    first = first,
    rest = function(a) return List:new(a:slice(2)) end,
    ['empty?'] = function(a) return a==Nil or #a == 0 end,
    count =  function(a) return #a end,
    apply = apply,
    map = map,
    conj = conj,

    meta = meta,
    ['with-meta'] = with_meta,
    atom = function(a) return types.Atom:new(a) end,
    ['atom?'] = types._atom_Q,
    deref = function(a) return a.val end,
    ['reset!'] = function(a,b) a.val = b; return b end,
    ['swap!'] = swap_BANG,
}

return M

