#! /usr/bin/env crystal run

require "./readline"
require "./reader"
require "./printer"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

def read(str)
  read_str str
end

def eval(x)
    x
end

def print(result)
  pr_str(result, true)
end

def rep(str)
  print(eval(read(str)))
end

while line = my_readline("user> ")
  begin
    puts rep(line)
  rescue e
    STDERR.puts e
  end
end
