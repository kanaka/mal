#! /usr/bin/env crystal run

require "readline"
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
  ->(args : Array(Mal::Type)) {
    x, y = args[0].unwrap, args[1].unwrap
    eval_error "invalid arguments" unless x.is_a?(Int64) && y.is_a?(Int64)
    Mal::Type.new func.call(x, y)
  }
end

REPL_ENV = Mal::Env.new nil
REPL_ENV.set("+", Mal::Type.new num_func(->(x : Int64, y : Int64) { x + y }))
REPL_ENV.set("-", Mal::Type.new num_func(->(x : Int64, y : Int64) { x - y }))
REPL_ENV.set("*", Mal::Type.new num_func(->(x : Int64, y : Int64) { x * y }))
REPL_ENV.set("/", Mal::Type.new num_func(->(x : Int64, y : Int64) { x // y }))

module Mal
  extend self

  def read(str)
    read_str str
  end

  def eval(ast, env)
    puts "EVAL: #{print(ast)}" if env.get("DEBUG-EVAL")

    val = ast.unwrap

    case val
    when Mal::Symbol
      e = env.get(val.str)
      eval_error "'#{val.str}' not found" unless e
      return e
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

      head = list.first.unwrap
      if head.is_a? Mal::Symbol
         a0sym = head.str
      else
         a0sym = ""
      end
      case a0sym
      when "def!"
        eval_error "wrong number of argument for 'def!'" unless list.size == 3
        a1 = list[1].unwrap
        eval_error "1st argument of 'def!' must be symbol: #{a1}" unless a1.is_a? Mal::Symbol
        return Mal::Type.new env.set(a1.str, eval(list[2], env))
      when "let*"
        eval_error "wrong number of argument for 'def!'" unless list.size == 3

        bindings = list[1].unwrap
        eval_error "1st argument of 'let*' must be list or vector" unless bindings.is_a? Array
        eval_error "size of binding list must be even" unless bindings.size.even?

        new_env = Mal::Env.new env
        bindings.each_slice(2) do |binding|
          key, value = binding
          name = key.unwrap
          eval_error "name of binding must be specified as symbol #{name}" unless name.is_a? Mal::Symbol
          new_env.set(name.str, eval(value, new_env))
        end

        return eval(list[2], new_env)
      else
        f = eval(list.first, env).unwrap
        case f
        when Mal::Func
          args = list[1..-1].map { |n| eval(n, env).as(Mal::Type) }
          return f.call args
        else
          eval_error "expected function as the first argument: #{f}"
        end
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
