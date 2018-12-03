#! /usr/bin/env crystal run

require "readline"
require "./reader"
require "./printer"
require "./types"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

module Mal
  extend self

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

  def eval_ast(a, env)
    return a.map { |n| eval(n, env).as(Mal::Type) } if a.is_a? Mal::List
    return a unless a

    ast = a.unwrap
    case ast
    when Mal::Symbol
      if env.has_key? ast.str
        env[ast.str]
      else
        eval_error "'#{ast.str}' not found"
      end
    when Mal::List
      ast.each_with_object(Mal::List.new) { |n, l| l << eval(n, env) }
    when Mal::Vector
      ast.each_with_object(Mal::Vector.new) { |n, l| l << eval(n, env) }
    when Mal::HashMap
      ast.each { |k, v| ast[k] = eval(v, env) }
      ast
    else
      ast
    end
  end

  def read(str)
    read_str str
  end

  def eval(t, env)
    Mal::Type.new case ast = t.unwrap
    when Mal::List
      return gen_type Mal::List if ast.empty?

      f = eval_ast(ast.first, env)
      ast.shift(1)
      args = eval_ast(ast, env)

      if f.is_a?(Mal::Func)
        f.call(args)
      else
        eval_error "expected function symbol as the first symbol of list"
      end
    else
      eval_ast(t, env)
    end
  end

  def print(result)
    pr_str(result, true)
  end

  def rep(str)
    print(eval(read(str), REPL_ENV))
  end
end

REPL_ENV = {
  "+" => Mal.num_func(->(x : Int64, y : Int64) { x + y }),
  "-" => Mal.num_func(->(x : Int64, y : Int64) { x - y }),
  "*" => Mal.num_func(->(x : Int64, y : Int64) { x * y }),
  "/" => Mal.num_func(->(x : Int64, y : Int64) { x / y }),
} of String => Mal::Func

while line = Readline.readline("user> ", true)
  begin
    puts Mal.rep(line)
  rescue e : Mal::RuntimeException
    STDERR.puts "Error: #{pr_str(e.thrown, true)}"
  rescue e
    STDERR.puts "Error: #{e}"
  end
end
