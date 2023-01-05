Reader = require "reader"
function READ(prompt)
  io.write(prompt)
  local v = io.read()
  if v == nil then
    io.write('\n')
    return nil
  end
  return  Reader.read_str(  v)
end
function EVAL(a)
  return a
end
function PRINT(a)
  print(Reader.stringfy_val(a, true))
end


function main()
    local line = ''
    while true do
      line = READ('user> ')
      if line == nil then
        break
      end
      PRINT(EVAL(line)) 
    end
end

main()
