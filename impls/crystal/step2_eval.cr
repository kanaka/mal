#! /usr/bin/env crystal run

require "readline"
require "./reader"
require "./printer"
require "./types"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

def eval_error(msg)
  raise Mal::EvalException.new msg
end

def num_func(func)
  ->(args : Array(Mal::Type)) {
    x, y = args[0].unwrap, args[1].unwrap
    eval_error "invalid arguments" unless x.is_a?(Int64) && y.is_a?(Int64)
    Mal::Type.new func.call(x, y)
  }
end

REPL_ENV = {
  "+" => Mal::Type.new(num_func(->(x : Int64, y : Int64) { x + y })),
  "-" => Mal::Type.new(num_func(->(x : Int64, y : Int64) { x - y })),
  "*" => Mal::Type.new(num_func(->(x : Int64, y : Int64) { x * y })),
  "/" => Mal::Type.new(num_func(->(x : Int64, y : Int64) { x // y })),
} of String => Mal::Type

module Mal
  extend self

  def read(str)
    read_str str
  end

  def eval(ast, env)
    # puts "EVAL: #{print(ast)}"

    val = ast.unwrap

    case val
    when Mal::Symbol
      eval_error "'#{val.str}' not found" unless env.has_key? val.str
      return env[val.str]
    when Mal::Vector
      new_vec = val.each_with_object(Mal::Vector.new) { |n, l| l << eval(n, env) }
      return Mal::Type.new new_vec
    when Mal::HashMap
      new_map = Mal::HashMap.new
      val.each { |k, v| new_map[k] = eval(v, env) }
      return Mal::Type.new new_map
    when Mal::List
      list = val
      return ast if list.empty?

        f = eval(list.first, env).unwrap
        case f
        when Mal::Func
          args = list[1..-1].map { |n| eval(n, env).as(Mal::Type) }
          return f.call args
        else
          eval_error "expected function as the first argument: #{f}"
        end

    else
      return Mal::Type.new val
    end
  end

  def print(result)
    pr_str(result, true)
  end

  def rep(str)
    print(eval(read(str), REPL_ENV))
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
