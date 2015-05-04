#! /usr/bin/env crystal run

require "./readline"
require "./reader"
require "./printer"
require "./types"

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

$repl_env = {
  "+" => num_func(->(x : Int32, y : Int32){ x + y }),
  "-" => num_func(->(x : Int32, y : Int32){ x - y }),
  "*" => num_func(->(x : Int32, y : Int32){ x * y }),
  "/" => num_func(->(x : Int32, y : Int32){ x / y }),
} of String => Mal::Type

def eval_ast(ast, env)
  case ast
  when Mal::Symbol
    if env.has_key? ast.val
      env[ast.val]
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
    case ast
    when Mal::List
      eval_error "empty list" if ast.empty?

      f = eval_ast(ast[0], env)
      ast.shift(1)
      args = eval_ast(ast, env).each_with_object([] of Mal::Type){|e, a| a << e}

      if f.is_a?(Mal::Func)
        f.call(args)
      else
        eval_error "expected function symbol as the first symbol of list"
      end
    else
      eval_ast(ast, env)
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
