#! /usr/bin/env crystal run

require "./readline"
require "./reader"
require "./printer"
require "./types"
require "./env"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

def eval_error(msg)
  raise Mal::EvalException.new msg
end

def num_func(func)
  -> (args : Array(Mal::Type)) {
    x, y = args[0].unwrap, args[1].unwrap
    eval_error "invalid arguments" unless x.is_a?(Int32) && y.is_a?(Int32)
    Mal::Type.new func.call(x, y)
  }
end

$repl_env = Mal::Env.new nil
$repl_env.set("+", Mal::Type.new num_func(->(x : Int32, y : Int32){ x + y }))
$repl_env.set("-", Mal::Type.new num_func(->(x : Int32, y : Int32){ x - y }))
$repl_env.set("*", Mal::Type.new num_func(->(x : Int32, y : Int32){ x * y }))
$repl_env.set("/", Mal::Type.new num_func(->(x : Int32, y : Int32){ x / y }))

module Mal
  extend self

  def eval_ast(a, env)
    return a.map{|n| eval(n, env) } if a.is_a? Array

    Mal::Type.new case ast = a.unwrap
    when Mal::Symbol
      if e = env.get(ast.str)
        e
      else
        eval_error "'#{ast.str}' not found"
      end
    when Mal::List
      ast.each_with_object(Mal::List.new){|n, l| l << eval(n, env)}
    when Mal::Vector
      ast.each_with_object(Mal::Vector.new){|n, l| l << eval(n, env)}
    when Mal::HashMap
      new_map = Mal::HashMap.new
      ast.each{|k, v| new_map[k] = eval(v, env)}
      new_map
    else
      ast
    end
  end

  def read(str)
    read_str str
  end

  def eval(t, env)
    ast = t.unwrap

    return eval_ast(t, env) unless ast.is_a?(Mal::List)

    eval_error "empty list" if ast.empty?

    sym = ast.first.unwrap
    eval_error "first element of list must be a symbol" unless sym.is_a?(Mal::Symbol)

    Mal::Type.new case sym.str
    when "def!"
      eval_error "wrong number of argument for 'def!'" unless ast.size == 3
      a1 = ast[1].unwrap
      eval_error "1st argument of 'def!' must be symbol" unless a1.is_a?(Mal::Symbol)
      env.set(a1.str, eval(ast[2], env) as Mal::Type)
    when "let*"
      eval_error "wrong number of argument for 'def!'" unless ast.size == 3

      bindings = ast[1].unwrap
      eval_error "1st argument of 'let*' must be list or vector" unless bindings.is_a?(Array)
      eval_error "size of binding list must be even" unless bindings.size.even?

      new_env = Mal::Env.new env
      bindings.each_slice(2) do |binding|
        name, value = binding[0].unwrap, binding[1]
        eval_error "name of binding must be specified as symbol" unless name.is_a?(Mal::Symbol)
        new_env.set(name.str, eval(value, new_env))
      end

      eval(ast[2], new_env)
    else
      f = eval_ast(ast.first, env)
      ast.shift(1)
      args = eval_ast(ast, env)

      if f.is_a?(Mal::Type) && (f2 = f.unwrap).is_a?(Mal::Func)
        f2.call(args as Array(Mal::Type))
      else
        eval_error "expected function symbol as the first symbol of list"
      end
    end
  end

  def print(result)
    pr_str(result, true)
  end

  def rep(str)
    print(eval(read(str), $repl_env))
  end
end

while line = my_readline("user> ")
  begin
    puts Mal.rep(line)
  rescue e
    STDERR.puts e
  end
end
