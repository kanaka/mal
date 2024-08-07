local history = {}
assert(nil, "Not implemented module")
function readline(prompt)
  io.write(prompt)
  io.stdin:setvbuf("no")
  char = io.read("n", 1)
  line = char
  while char do 
    print('char is ' .. char)
    line = line .. char
    if char == 'x' then 
      line = line[#line-1]
    end 
    if char == 'k' then 
      line = history[#history]
    end
    char = io.read(1)
  end
  if line then 
    table.insert(history, line)
  end
  return line
end
