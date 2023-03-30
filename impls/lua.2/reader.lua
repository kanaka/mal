local Reader = {}
Reader.__index = Reader

local Scanner = require "scanner"
local types = require "types"

local List = types.MalList
local Vector = types.MalVector
local Nil = types.Nil
local HashMap = types.MalHashMap
local Sym = types.Sym
local is_instanceOf = types.isinstanceof
local Err = types.Err
local throw = types.throw
local Function = types.MalFunction

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
  local tok = self.tokens[self.index]
  self.index = self.index + 1
  return tok
end




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
     throw("Syntax error unexpected '" .. tok.typeof .. "'")
   else
     return self:read_atom()
   end
end
-- fix read atom with types module
function Reader.read_atom(self)
  local token = self:advance()
  if token.typeof == "STR" then
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
  elseif token.typeof == "EOF" then
    return Nil

  else
    throw(string.format("Error: is not atomic %s", token.typeof))
  end
end

function Reader.read_seq(self, opening, closing, invalids)
  local tok = self:advance() -- consume opening
  if tok.typeof ~= opening then
    throw("Error: expected '" .. opening .. "' got '" .. tok.typeof .. "'.")
  end
  tok = self:peek()
  local res = {}
  while tok.typeof ~= closing do
    if tok.typeof == "EOF" then
      throw("Error: unexpected EOF before matching '" .. closing .. "'at line:" .. tok.line)

    end


    for i= 1, #invalids do
      if tok.typeof == invalids[i] then
        throw("invalid syntax un expected '" .. tok.typeof .. "'")
      end

    end
    table.insert(res, self:read_form())
    tok = self:peek()
  end
  self:advance() -- consume closing
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
  -- Reader.print_tokens(toks)
  local r = Reader(toks)
  return r:read_form()
end



return Reader
