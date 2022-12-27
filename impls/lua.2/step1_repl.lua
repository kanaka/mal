reader = require "reader"
function READ(prompt)
  io.write(prompt)
  local v = io.read()
  print(v)
  return  reader.read_str(  v)
end
function EVAL(a)
  return a
end
function PRINT(a)
  reader.print_tokens(a)
end


function main()
    while true do
      line = READ('user> ')
      if not line then
        break
      end
      PRINT(EVAL(line)) 
    end
end

main()
