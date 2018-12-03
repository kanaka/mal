#! /usr/bin/env crystal run

require "readline"
require "./reader"
require "./printer"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

module Mal
  extend self

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
end

while line = Readline.readline("user> ", true)
  begin
    puts Mal.rep(line)
  rescue e : Mal::RuntimeException
    STDERR.puts "Error: #{pr_str(e.thrown, true)}"
  rescue e
    STDERR.puts "Error: #{e}"
  end
end
