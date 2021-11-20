require "readline"

module Mal
  extend self

  def READ(input)
    input
  end

  def EVAL(input)
    input
  end

  def PRINT(input)
    input
  end

  def rep(input)
    PRINT(EVAL(READ(input)))
  end
end

while input = Readline.readline("user> ")
  puts Mal.rep(input)
end
