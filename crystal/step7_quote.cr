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
    when Array(Mal::Type)
      val.map{|n| eval(n, env)}
    when Mal::HashMap
      val.each{|k, v| val[k] = eval(v, env)}
      val
    else
      val
    end
  end

  def read(str)
    read_str str
  end

  macro is_pair(list)
    {{list}}.is_a?(Array) && !{{list}}.empty?
  end

  def quasiquote(ast)
    list = ast.unwrap

    unless is_pair(list)
      return Mal::Type.new(
        Mal::List.new << gen_type(Mal::Symbol, "quote") << ast
      )
    end

    head = list.first.unwrap

    case
    # ("unquote" ...)
    when head.is_a?(Mal::Symbol) && head.str == "unquote"
      list[1]
    # (("splice-unquote" ...) ...)
    when is_pair(head) && (arg0 = head.first.unwrap).is_a?(Mal::Symbol) && arg0.str == "splice-unquote"
      tail = Mal::Type.new list[1..-1].each_with_object(Mal::List.new){|e,l| l << e}
      Mal::Type.new(
        Mal::List.new << gen_type(Mal::Symbol, "concat") << head[1] << quasiquote(tail)
      )
    else
      tail = Mal::Type.new list[1..-1].each_with_object(Mal::List.new){|e,l| l << e}
      Mal::Type.new(
        Mal::List.new << gen_type(Mal::Symbol, "cons") << quasiquote(list.first) << quasiquote(tail)
      )
    end
  end

  macro invoke_list(l, env)
    f = eval({{l}}.first, {{env}}).unwrap
    args = eval_ast({{l}}[1..-1].each_with_object(Mal::List.new){|i, l| l << i}, {{env}})
    case f
    when Mal::Closure
      ast = f.ast
      {{env}} = Mal::Env.new(f.env, f.params, args)
      next # TCO
    when Mal::Func
      return f.call args
    else
      eval_error "expected function as the first argument"
    end
  end

  def eval(ast, env)
    # 'next' in 'do...end' has a bug in crystal 0.7.1
    # https://github.com/manastech/crystal/issues/659
    while true
      list = ast.unwrap

      return eval_ast(ast, env) unless list.is_a? Mal::List
      return gen_type Mal::List if list.empty?

      head = list.first.unwrap

      unless head.is_a? Mal::Symbol
        return invoke_list(list, env)
      end

      return Mal::Type.new case head.str
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

          ast, env = list[2], new_env
          next # TCO
        when "do"
          if list.empty?
            ast = Mal::Type.new nil
            next
          end

          eval_ast(list[1..-2].each_with_object(Mal::List.new){|i,l| l << i}, env)
          ast = list.last
          next # TCO
        when "if"
          ast = unless eval(list[1], env).unwrap
            list.size >= 4 ? list[3] : Mal::Type.new(nil)
          else
            list[2]
          end
          next # TCO
        when "fn*"
          params = list[1].unwrap
          unless params.is_a?(Mal::List) || params.is_a?(Mal::Vector)
            eval_error "'fn*' parameters must be list"
          end
          Mal::Closure.new(list[2], params, env, func_of(env, params, list[2]))
        when "quote"
          list[1]
        when "quasiquote"
          ast = quasiquote list[1]
          next # TCO
        else
          invoke_list(list, env)
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

$repl_env = Mal::Env.new nil
Mal::NS.each{|k,v| $repl_env.set(k, Mal::Type.new(v))}
$repl_env.set("eval", Mal::Type.new -> (args: Array(Mal::Type)){ Mal.eval(args[0], $repl_env) })
Mal.rep "(def! not (fn* (a) (if a false true)))"
Mal.rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
$argv = Mal::List.new
$repl_env.set("*ARGV*", Mal::Type.new $argv)

unless ARGV.empty?
  if ARGV.size > 1
    ARGV[1..-1].each do |a|
      $argv << Mal::Type.new(a)
    end
  end

  begin
    Mal.rep "(load-file \"#{ARGV[0]}\")"
  rescue e
    STDERR.puts e
  end
  exit
end

while line = my_readline("user> ")
  begin
    puts Mal.rep(line)
  rescue e
    STDERR.puts e
  end
end
