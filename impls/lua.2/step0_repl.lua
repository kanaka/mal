function READ(prompt)
  io.write(prompt)
  return io.read()
end
function EVAL(a)
  return a
end
function PRINT(a)
  print(a)
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
