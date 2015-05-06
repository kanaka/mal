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
    x, y = args[0], args[1]
    eval_error "invalid arguments" unless x.is_a?(Int32) && y.is_a?(Int32)
    func.call(x, y) as Mal::Type
  } as Mal::Func
end

$repl_env = Mal::Env.new nil
$repl_env.set("+", num_func(->(x : Int32, y : Int32){ x + y }))
$repl_env.set("-", num_func(->(x : Int32, y : Int32){ x - y }))
$repl_env.set("*", num_func(->(x : Int32, y : Int32){ x * y }))
$repl_env.set("/", num_func(->(x : Int32, y : Int32){ x / y }))

def eval_ast(ast, env)
  case ast
  when Mal::Symbol
    if e = env.get(ast.val)
      e
    else
      eval_error "'#{ast.val}' not found"
    end
  when Mal::List
    # ast.each_with_object(Mal::List.new){|n, l| l << eval(n, env) as Mal::Type}
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

  eval_error "empty list" if ast.empty?

  sym = ast.first
  eval_error "first element of list must be a symbol" unless sym.is_a?(Mal::Symbol)

  case sym.val
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
  else
    f = eval_ast(sym, env)
    ast.shift(1)
    args = eval_ast(ast, env).each_with_object([] of Mal::Type){|e, a| a << e}

    if f.is_a?(Mal::Func)
      f.call(args)
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

while line = my_readline("user> ")
  begin
    puts rep(line)
  rescue e
    STDERR.puts e
  end
end
