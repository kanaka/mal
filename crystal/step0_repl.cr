#! /usr/bin/env crystal run

require "./readline"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

def read(x)
    x
end

def eval(x)
    x
end

def print(x)
    x
end

def rep(x)
    read(eval(print(x)))
end

while line = my_readline("user> ")
    puts rep(line)
end
