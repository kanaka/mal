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
  return char == '(' or char == ')'  or 
         char == '[' or char == ']'  or 
         char == '{' or char == '}'  or
         char == '\'' or char == '`' or char == '"' or
         char == '@' or char == '~' or 
         char == '^' or char == '\0'
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
    self:advance()
  end
  print(string.format("peeeked %s", self:peek()))
  if self:isAtEnd() then
    print("Error unterminated string at line %d", self.line)
    return
  end

  self:advance() -- closing "

  val = string.sub(self.source, self.start+1, self.index-2) -- trimmed opening and closing "

  table.insert(self.tokens, Token("STR", val, self.line))

end

function Scanner.scanToken(self)
  char = self:advance()
  
  -- print(string.format("b c:%s, i:%d", char, self.index))
  if char == ' ' or char == ',' or char == '\n' then
    if char == '\n' then
      self.line = self.line + 1
    end
  elseif char == '~' then
    if self:match('@') then
      table.insert(self.tokens, Token("TILDE_AT", "", self.line))
    else 
      table.insert(self.tokens, Token("TILDE", "", self.line))
    end
  elseif char == '[' then
    table.insert(self.tokens, Token("L_BRA", "", self.line))
  elseif char == ']' then
    table.insert(self.tokens, Token("R_BRA", "", self.line))
  elseif char == '(' then
    table.insert(self.tokens, Token("L_PAR", "", self.line))
  elseif char == ')' then
    table.insert(self.tokens, Token("R_PAR", "", self.line))
  elseif char == '{' then
    table.insert(self.tokens, Token("L_CURLY", "", self.line))
  elseif char == '}' then
    table.insert(self.tokens, Token("R_CURLY", "", self.line))
  elseif char == '\'' then
    table.insert(self.tokens, Token("TICK", "", self.line))
  elseif char == '`' then
    table.insert(self.tokens, Token("BACKTICK", "", self.line))
  elseif char == '^' then
    table.insert(self.tokens, Token("CARROT", "", self.line))
  elseif char == '@' then
    table.insert(self.tokens, Token("AT", "", self.line))
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

return Scanner
