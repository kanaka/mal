local Reader = {}
Reader.__index = Reader

local types = require "types"

local List = types.MalList
local Vector = types.MalVector
local Nil = types.Nil
local HashMap = types.MalHashMap
local Sym = types.Sym
local is_instanceOf = types.isinstanceof
local Err = types.Err

setmetatable(Reader, {
  __call = function (cls, ...)
    return cls.new(...)
  end,
})

function Reader.new(tokens)
  local self = setmetatable({}, Reader)
  self.tokens = tokens
  self.index  = 1
  return self
end

function Reader.peek(self)
  return self.tokens[self.index]
end

function Reader.advance(self)
  tok = self.tokens[self.index]
  self.index = self.index + 1
  return tok
end



Scanner = require "scanner"

function Reader.read_form(self)
   local tok = self:peek()
   if tok.typeof == '(' then
     return List.new(   self:read_seq('(', ')', { '}', ']' }))
   elseif tok.typeof == '[' then
     return Vector.new( self:read_seq('[', ']', { ')' , '}'}))
   elseif tok.typeof == '{' then
     return HashMap.new(table.unpack(self:read_seq('{', '}', { ')' , ']'})))
   elseif tok.typeof == 'CMT' then
     self:advance()
     return Nil
   elseif tok.typeof == '@' then
     self:advance()
     return List.new({Sym.new('deref'), self:read_form()})
   elseif tok.typeof == '~@' then
     self:advance()
     return List.new({Sym.new('splice-unquote'), self:read_form()})
   elseif tok.typeof == '`' then
     self:advance()
     return List.new({Sym.new('quasiquote'), self:read_form()})
   elseif tok.typeof == '~' then
     self:advance()
     return List.new({Sym.new('unquote'), self:read_form()})
   elseif tok.typeof == "'" then
     self:advance()
     return List.new({Sym.new('quote'), self:read_form()})
   elseif tok.typeof == '^' then
     self:advance()
     local meta = self:read_form()
     return List.new({Sym.new('with-meta'), self:read_form(), meta})
   elseif tok.typeof == ')' or tok.typeof == ']' or tok.typeof == '}' then
     return Err.new("Syntax error")
   else
     return self:read_atom()
   end
end
-- fix read atom with types module
function Reader.read_atom(self)
  local token = self:advance()
  if token.typeof == "STR" then
    if token.val == nil then
      return Err.new("unterminated string")
    end
    return token.val
  elseif token.typeof == "SYM" then
    if token.val == "true" then
      return true
    elseif token.val == "false" then
      return false
    elseif token.val == 'nil' then
      return Nil
    elseif string.match(token.val, '^-?%d+%.?%d*$') then
      return tonumber(token.val)
    else
      return Sym.new(token.val)
    end

  else
    print(string.format("Error: is not atomic %s", token.typeof))
    return Err.new("internal error")
  end
end

function Reader.read_seq(self, opening, closing, invalids)
  local tok = self:advance() -- consume opening
  if tok.typeof ~= opening then 
    error("Error: expected '" .. opening .. "' got '" .. tok.typeof .. "'.")
  end
  tok = self:peek()
  local res = {} 
  while tok.typeof ~= closing do
    if tok.typeof == "EOF" then 
      print("Error: unexpected EOF before matching '" .. closing .. "'.")
      return {}
    end


    for i= 1, #invalids do 
      if tok.typeof == invalids[i] then
        return Err.new("Error: invalid syntax")
      end

    end
    table.insert(res, self:read_form())
    tok = self:peek()
  end
  self:advance() -- consume closing
  return res
end


function Reader.stringfy_val(val, readably)
  local res = ''
  if is_instanceOf(val, Vector) then
    res = res .. '['
    for i=1, #val do
      res = res .. Reader.stringfy_val(val[i],readably)
      if i ~= #val then
        res = res .. " "
      end
    end
    res = res .. ']'
  elseif is_instanceOf(val, List) then
    res = res .. '('
    for i=1, #val do
      res = res .. Reader.stringfy_val(val[i],readably)
      if i ~= #val then
        res = res .. " "
      end
    end
    res = res .. ')'
  elseif is_instanceOf(val, HashMap) then
    res = res .. '{'
    for i,v in pairs(val) do
      res = res .. Reader.stringfy_val(i, readably) .. " " .. Reader.stringfy_val(v,readably)
      res = res .. " "
    end
    if #res > 1 then
      res = string.sub(res, 1, #res-1) -- trim last space
    end
    res = res .. '}'
  
  elseif is_instanceOf(val, Sym) then
    return val.val 
  elseif is_instanceOf(val, Err) then
    return "Error:" .. Scanner.unescape(val.val)
  elseif type(val) == "string" then
    if readably then 
      res = Scanner.unescape(val)
    else 
      res = Scanner.escape(val)
    end
  elseif type(val) == "number" then
    res = tostring(val)
  elseif val == Nil then
    res = "nil"
  elseif type(val) == "boolean" then 
    res = tostring(val)
  else
    error(string.format("Error: unknown type %s", val))

  end
  return res
end

function Reader.print_tokens(tokens)
  for i,v in ipairs(tokens) do print(i,v:tostring()) end
end

function Reader.read_str(a)
  local s = Scanner(a)
  local toks = s:scanTokens()
  if not toks then
    return Err.new("No token")
  end
  local r = Reader(toks)
  return r:read_form()
end



return Reader
