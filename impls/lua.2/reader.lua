local reader = {}
Scanner = require "scanner"




function reader.print_tokens(tokens)
  for i,v in ipairs(tokens) do print(i,v:tostring()) end
end

function reader.read_str(a)
  s = Scanner(a)
  print(string.format("Scanner source: %s", s.source))
  toks = s:scanTokens()
  return toks
end



return reader
