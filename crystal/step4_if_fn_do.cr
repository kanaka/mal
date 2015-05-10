#! /usr/bin/env crystal run

require "./readline"
require "./reader"
require "./printer"
require "./types"
require "./env"
require "./core"

# Note:
# Employed downcase names because Crystal prohibits uppercase names for methods

def eval_error(msg)
  raise Mal::EvalException.new msg
end

def func_of(env, binds, body) : Mal::Type
  -> (args : Array(Mal::Type)) {
    new_env = Mal::Env.new(env, binds, args)
    eval(body, new_env) as Mal::Type
  } as Mal::Func
end

def eval_ast(ast, env)
  case ast
  when Mal::Symbol
    if e = env.get(ast.val)
      e
    else
      eval_error "'#{ast.val}' not found"
    end
  when Mal::List
    ast.map{|n| eval(n, env) as Mal::Type}
  when Mal::Vector
    ast.each_with_object(Mal::Vector.new){|n, l| l << eval(n, env)}
  when Mal::HashMap
    ast.each{|k, v| ast[k] = eval(v, env)}
  else
    ast
  end
end

def read(str)
  read_str str
end

def eval(ast, env)
  return eval_ast(ast, env) unless ast.is_a?(Mal::List)

  return Mal::List.new if ast.empty?

  head = ast.first
  case head
  when Mal::Symbol
    case head.val
    when "def!"
      eval_error "wrong number of argument for 'def!'" unless ast.size == 3
      a1 = ast[1]
      eval_error "1st argument of 'def!' must be symbol" unless a1.is_a?(Mal::Symbol)
      env.set(a1.val, eval(ast[2], env) as Mal::Type)
    when "let*"
      eval_error "wrong number of argument for 'def!'" unless ast.size == 3

      bindings = ast[1]
      eval_error "1st argument of 'let*' must be list or vector" unless bindings.is_a?(Array)
      eval_error "size of binding list must be even" unless bindings.size.even?

      new_env = Mal::Env.new env
      bindings.each_slice(2) do |binding|
        name, value = binding
        eval_error "name of binding must be specified as symbol" unless name.is_a?(Mal::Symbol)
        new_env.set(name.val, eval(value, new_env))
      end

      eval(ast[2], new_env)
    when "do"
      ast.shift(1)
      eval_ast(ast, env).last
    when "if"
      cond = eval(ast[1], env)
      case cond
      when Nil
        ast.size >= 4 ? eval(ast[3], env) : nil
      when false
        ast.size >= 4 ?  eval(ast[3], env) : nil
      else
        eval(ast[2], env)
      end
    when "fn*"
      # Note:
      # If writing lambda expression here directly, compiler will fail to infer type of 'ast'. (Error 'Nil for empty?')
      func_of(env, ast[1], ast[2])
    else
      f = eval_ast(head, env)
      eval_error "expected function symbol as the first symbol of list" unless f.is_a?(Mal::Func)
      ast.shift(1)
      f.call eval_ast(ast, env).each_with_object([] of Mal::Type){|e, a| a << e}
    end
  else
    f = eval(head, env)
    eval_error "expected function symbol as the first symbol of list" unless f.is_a?(Mal::Func)
    ast.shift(1)
    f.call eval_ast(ast, env).each_with_object([] of Mal::Type){|e, a| a << e}
  end
end

def print(result)
  pr_str(result, true)
end

def rep(str)
  print(eval(read(str), $repl_env))
end

$repl_env = Mal::Env.new nil
Mal::NS.each{|k,v| $repl_env.set(k, v)}

while line = my_readline("user> ")
  begin
    puts rep(line)
  rescue e
    STDERR.puts e
  end
end
