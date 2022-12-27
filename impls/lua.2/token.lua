Token = {}
Token.__index = Token

setmetatable(Token, {
  __call = function (cls, ...)
    return cls.new(...)
  end,
})

function Token.new(typeof, val, line)
  local self = setmetatable({}, Token)
  self.typeof = typeof
  self.val = val
  self.line = line
  return self
end

function Token.tostring(self)
  -- https://youtu.be/S4eNl1rA1Ns?t=1435
  return string.format("Token: %s:%s at %d", self.typeof, self.val, self.line)
end


return Token
