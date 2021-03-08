local utils = require('utils')
local types = require('types')
local reader = require('reader')
local printer = require('printer')
local readline = require('readline')

local Nil, List, HashMap, _pr_str = types.Nil, types.List, types.HashMap, printer._pr_str

local M = {}

-- string functions

function pr_str(...)
    return table.concat(
        utils.map(function(e) return _pr_str(e, true) end,
                  table.pack(...)), " ")
end

function str(...)
    return table.concat(
        utils.map(function(e) return _pr_str(e, false) end,
                  table.pack(...)), "")
end

function prn(...)
    print(table.concat(
        utils.map(function(e) return _pr_str(e, true) end,
                  table.pack(...)), " "))
    io.flush()
    return Nil
end

function println(...)
    print(table.concat(
        utils.map(function(e) return _pr_str(e, false) end,
                  table.pack(...)), " "))
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
    return types._assoc_BANG(types.copy(hm), ...)
end

function dissoc(hm, ...)
    return types._dissoc_BANG(types.copy(hm), ...)
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
    local arg = table.pack(...)
    local new_lst = {}
    for i = 1, #arg do
        for j = 1, #arg[i] do
            table.insert(new_lst, arg[i][j])
        end
    end
    return List:new(new_lst)
end

function vec(a)
    return types.Vector:new(a)
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

function rest(a)
    if a == Nil then
        return List:new()
    else
        return List:new(a:slice(2))
    end
end

function apply(f, ...)
    local arg = table.pack(...)
    if types._malfunc_Q(f) then
        f = f.fn
    end
    local args = concat(types.slice(arg, 1, #arg-1),
                        arg[#arg])
    return f(table.unpack(args))
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
    local args = List:new(table.pack(...))
    table.insert(args, 1, atm.val)
    atm.val = f(table.unpack(args))
    return atm.val
end

local function conj(obj, ...)
    local arg = table.pack(...)
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

local function seq(obj, ...)
    if obj == Nil or #obj == 0 then
        return Nil
    elseif types._list_Q(obj) then
        return obj
    elseif types._vector_Q(obj) then
        return List:new(obj)
    elseif types._string_Q(obj) then
        local chars = {}
        for i = 1, #obj do
            chars[#chars+1] = string.sub(obj,i,i)
        end
        return List:new(chars)
    end
    return Nil
end

local function lua_to_mal(a)
  if a == nil then
    return Nil
  elseif type(a) == "boolean" or type(a) == "number" or type(a) == "string" then
    return a
  elseif type(a) == "table" then
    local first_key, _ = next(a)
    if first_key == nil then
      return List:new({})
    elseif type(first_key) == "number" then
      local list = {}
      for i, v in ipairs(a) do
        list[i] = lua_to_mal(v)
      end
      return List:new(list)
    else
      local hashmap = {}
      for k, v in pairs(a) do
        hashmap[lua_to_mal(k)] = lua_to_mal(v)
      end
      return HashMap:new(hashmap)
    end
  end
  return tostring(a)
end

local function lua_eval(str)
    local f, err = load("return "..str)
    if err then
        types.throw("lua-eval: can't load code: "..err)
    end
    return lua_to_mal(f())
end

M.ns = {
    ['='] =  types._equal_Q,
    throw = types.throw,

    ['nil?'] =  function(a) return a==Nil end,
    ['true?'] =  function(a) return a==true end,
    ['false?'] =  function(a) return a==false end,
    ['number?'] = function(a) return types._number_Q(a) end,
    symbol = function(a) return types.Symbol:new(a) end,
    ['symbol?'] = function(a) return types._symbol_Q(a) end,
    ['string?'] = function(a) return types._string_Q(a) and "\u{029e}" ~= string.sub(a,1,2) end,
    keyword = function(a)
        if types._keyword_Q(a) then
            return a
        else
            return "\u{029e}"..a
        end
    end,
    ['keyword?'] = function(a) return types._keyword_Q(a) end,
    ['fn?'] = function(a) return types._fn_Q(a) end,
    ['macro?'] = function(a) return types._macro_Q(a) end,

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
    ['time-ms'] = function() return math.floor(os.clock()*1000000) end,

    list = function(...) return List:new(table.pack(...)) end,
    ['list?'] = function(a) return types._list_Q(a) end,
    vector = function(...) return types.Vector:new(table.pack(...)) end,
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
    vec = vec,
    nth = nth,
    first = first,
    rest = rest,
    ['empty?'] = function(a) return a==Nil or #a == 0 end,
    count =  function(a) return #a end,
    apply = apply,
    map = map,
    conj = conj,
    seq = seq,

    meta = meta,
    ['with-meta'] = with_meta,
    atom = function(a) return types.Atom:new(a) end,
    ['atom?'] = types._atom_Q,
    deref = function(a) return a.val end,
    ['reset!'] = function(a,b) a.val = b; return b end,
    ['swap!'] = swap_BANG,

    ['lua-eval'] = lua_eval,
}

return M

