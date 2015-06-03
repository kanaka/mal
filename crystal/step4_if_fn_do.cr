#! /usr/bin/env crystal run

require "./readline"
require "./reader"
require "./printer"
require "./types"
require "./env"
require "./core"
require "./error"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

module Mal
  extend self

  def func_of(env, binds, body)
    -> (args : Array(Mal::Type)) {
      new_env = Mal::Env.new(env, binds, args)
      eval(body, new_env)
    } as Mal::Func
  end

  def eval_ast(ast, env)
    return ast.map{|n| eval(n, env) as Mal::Type} if ast.is_a? Mal::List

    val = ast.unwrap

    Mal::Type.new case val
    when Mal::Symbol
      if e = env.get(val.str)
        e
      else
        eval_error "'#{val.str}' not found"
      end
    when Mal::List
      val.each_with_object(Mal::List.new){|n, l| l << eval(n, env)}
    when Mal::Vector
      val.each_with_object(Mal::Vector.new){|n, l| l << eval(n, env)}
    when Mal::HashMap
      val.each{|k, v| val[k] = eval(v, env)}
      val
    else
      val
    end
  end

  def eval_invocation(list, env)
    f = eval(list.first, env).unwrap
    eval_error "expected function symbol as the first symbol of list" unless f.is_a? Mal::Func
    f.call eval_ast(list[1..-1].each_with_object(Mal::List.new){|i, l| l << i}, env)
  end

  def read(str)
    read_str str
  end

  def eval(ast, env)
    list = ast.unwrap

    return eval_ast(ast, env)          unless list.is_a? Mal::List
    return gen_type Mal::List if list.empty?

    head = list.first.unwrap

    Mal::Type.new case head
    when Mal::Symbol
      case head.str
      when "def!"
        eval_error "wrong number of argument for 'def!'" unless list.size == 3
        a1 = list[1].unwrap
        eval_error "1st argument of 'def!' must be symbol" unless a1.is_a? Mal::Symbol
        env.set(a1.str, eval(list[2], env))
      when "let*"
        eval_error "wrong number of argument for 'def!'" unless list.size == 3

        bindings = list[1].unwrap
        eval_error "1st argument of 'let*' must be list or vector" unless bindings.is_a? Array
        eval_error "size of binding list must be even" unless bindings.size.even?

        new_env = Mal::Env.new env
        bindings.each_slice(2) do |binding|
          key, value = binding
          name = key.unwrap
          eval_error "name of binding must be specified as symbol" unless name.is_a? Mal::Symbol
          new_env.set(name.str, eval(value, new_env))
        end

        eval(list[2], new_env)
      when "do"
        list.shift 1
        eval_ast(list, env).last
      when "if"
        cond = eval(list[1], env).unwrap
        case cond
        when Nil
          list.size >= 4 ? eval(list[3], env) : nil
        when false
          list.size >= 4 ?  eval(list[3], env) : nil
        else
          eval(list[2], env)
        end
      when "fn*"
        # Note:
        # If writing lambda expression here directly, compiler will fail to infer type of 'list'. (Error 'Nil for empty?')
        func_of(env, list[1].unwrap, list[2])
      else
        eval_invocation(list, env)
      end
    else
      eval_invocation(list, env)
    end
  end

  def print(result)
    pr_str(result, true)
  end

  def rep(str)
    print(eval(read(str), $repl_env))
  end
end

$repl_env = Mal::Env.new nil
Mal::NS.each{|k,v| $repl_env.set(k, Mal::Type.new(v))}
Mal.rep "(def! not (fn* (a) (if a false true)))"

while line = my_readline("user> ")
  begin
    puts Mal.rep(line)
  rescue e
    STDERR.puts e
  end
end
