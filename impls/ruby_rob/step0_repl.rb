def READ(string)
  string
end

def EVAL(string)
  string
end

def PRINT(string)
  string
end

def rep(string)
  PRINT(EVAL(READ(string)))
end

while true
  print 'users> '
  input = gets
  unless input
    puts
    break
  end
  puts rep(input)
end
