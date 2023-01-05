local Token = require('token')
local Scanner = {}
Scanner.__index = Scanner

setmetatable(Scanner, {
  __call = function (cls, ...)
  return cls.new(...)
end,
})

function Scanner.new(source)
  local self = setmetatable({}, Scanner)
  self.source = source
  self.line = 1
  self.index = 1
  self.start = 1
  self.tokens = {}
  return self
end

function Scanner.advance(self)
  ch = string.sub(self.source, self.index, self.index)
  self.index = self.index + 1
  return ch
end

function Scanner.peek(self)
  if self:isAtEnd() then 
    return '\0'
  end
  return string.sub(self.source, self.index, self.index)
end

function Scanner.isAtEnd(self)
  return self.index >  #self.source
end

function Scanner.is_special(char)
  return char == '(' or char == ')'  or char == '[' or char == ']'  or
         char == '{' or char == '}'  or char == '\'' or char == '`' or
         char == '"' or char == '@' or char == '~' or char == '^' or
         char == '\0'or char == ' ' or char == '\t' or char == '\n' or char == ','
end

function Scanner.escape(str)
  local target = string.byte('\\')
  local dq = string.byte('"')
  local nl = string.byte('n')
  local res = ''
  local idx = 1
  while idx <=  #str do 
    if str:byte(idx) == target and str:byte(idx+1) == dq then
        res = res .. '"'
        idx = idx + 1
    elseif str:byte(idx) == target and str:byte(idx+1) == nl then
        res = res .. '\n'
        idx = idx + 1
    elseif str:byte(idx) == target and str:byte(idx+1) == target then
        res = res .. '\\'
        idx = idx + 1
    else
       res = res .. str:sub(idx,idx)
    end

    idx = idx + 1
 
  end
  for idx = 1, #str do 
 end
  return res
end
function Scanner.unescape(str)
  local nl = string.byte('\n')
  local bs = string.byte('\\')
  local dq = string.byte('"')
  local res = '"'
  local idx = 1
  while idx <= #str do
    if str:byte(idx) == nl then
      res = res .. '\\n'
    elseif str:byte(idx) == bs then
      res = res .. '\\\\'
    elseif str:byte(idx) == dq then
      res = res .. '\\"'
    else
      res = res .. str:sub(idx,idx)
    end
    idx = idx + 1
  end
  res = res .. '"'
  return res
end


function Scanner.scanTokens(self)
  while not self:isAtEnd() do
    self.start = self.index
    self:scanToken()
  end
  table.insert(self.tokens,Token("EOF", "", self.line))
  return self.tokens 
end

function Scanner.match(self, char)
  if self:peek() ~= char then
    return false
  end
  self:advance()
  return true
end

function Scanner.string(self)
  while self:peek() ~= '"' and not(self:isAtEnd()) do
    if self:peek() == '\\' then 
      self:advance()
    end
    self:advance()
  end
  if self:isAtEnd() then
    print(string.format("Error unbalanced string at line %d", self.line))
    table.insert(self.tokens, Token("STR", nil, self.line))
    return
  end

  self:advance() -- closing "
  
  -- trimmed opening and closing "
  val = self.escape(string.sub(self.source, self.start+1, self.index-2))
  
  table.insert(self.tokens, Token("STR", val, self.line))

end

function Scanner.scanToken(self)
  char = self:advance()
  
  -- print(string.format("b c:%s, i:%d", char, self.index))
  if char == ' ' or char == ',' or char == '\n' or char =='\t' then
    if char == '\n' then
      self.line = self.line + 1
    end
  elseif char == '~' then
    if self:match('@') then
      table.insert(self.tokens, Token("~@", "", self.line))
    else 
      table.insert(self.tokens, Token("~", "", self.line))
    end
  elseif char == '[' then
    table.insert(self.tokens, Token("[", "", self.line))
  elseif char == ']' then
    table.insert(self.tokens, Token("]", "", self.line))
  elseif char == '(' then
    table.insert(self.tokens, Token("(", "", self.line))
  elseif char == ')' then
    table.insert(self.tokens, Token(")", "", self.line))
  elseif char == '{' then
    table.insert(self.tokens, Token("{", "", self.line))
  elseif char == '}' then
    table.insert(self.tokens, Token("}", "", self.line))
  elseif char == '\'' then
    table.insert(self.tokens, Token("'", "", self.line))
  elseif char == '`' then
    table.insert(self.tokens, Token("`", "", self.line))
  elseif char == '^' then
    table.insert(self.tokens, Token("^", "", self.line))
  elseif char == '@' then
    table.insert(self.tokens, Token("@", "", self.line))
  elseif char == '"' then
    self:string()

  elseif char == ';' then
    while self:peek() ~= '\n' and not(self:isAtEnd()) do
      self:advance()
    end
    val = string.sub(self.source, self.start + 1, self.current)
    table.insert(self.tokens, Token("CMT", val, self.line))

  elseif not self.is_special(char)  then
    while not(self.is_special(self:peek())) do
      self:advance()
    end
    val = string.sub(self.source,self.start, self.index - 1)
    table.insert(self.tokens, Token("SYM", val, self.line))
  else 
    print(string.format("Error: unknown char: %s at %d, %d", char , line , idx) )
  end

end
--[[ examples
print(Scanner.escape("\\\\\\\\"))

print(Scanner.escape('\\"he"l\\nlo"'))

print(Scanner.unescape("\n"))
print(Scanner.unescape('"hello"'))
]]
return Scanner
