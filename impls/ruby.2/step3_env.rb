require "readline"

require_relative "env"
require_relative "errors"
require_relative "printer"
require_relative "reader"

module Mal
  extend self

  @repl_env = Env.new
  @repl_env.set(Types::Symbol.for('+'), -> (a, b) { a + b })
  @repl_env.set(Types::Symbol.for('-'), -> (a, b) { a - b })
  @repl_env.set(Types::Symbol.for('*'), -> (a, b) { a * b })
  @repl_env.set(Types::Symbol.for('/'), -> (a, b) { a / b })

  def READ(input)
    read_str(input)
  end

  def EVAL(ast, environment)
    if Types::List === ast && ast.size > 0
      case ast.first
      when Types::Symbol.for("def!")
        _, sym, val = ast
        environment.set(sym, EVAL(val, environment))
      when Types::Symbol.for("let*")
        e = Env.new(environment)
        _, bindings, val = ast

        unless Types::List === bindings || Types::Vector === bindings
          raise InvalidLetBindingsError
        end

        until bindings.empty?
          k, v = bindings.shift(2)

          raise InvalidLetBindingsError if k.nil?
          v = Types::Nil.instance if v.nil?

          e.set(k, EVAL(v, e))
        end

        if !val.nil?
          EVAL(val, e)
        else
          Types::Nil.instance
        end
      else
        evaluated = eval_ast(ast, environment)
        maybe_callable = evaluated.first

        if maybe_callable.respond_to?(:call)
          maybe_callable.call(*evaluated[1..])
        else
          raise NotCallableError, "Error! #{PRINT(maybe_callable)} is not callable."
        end
      end
    elsif Types::List === ast && ast.size == 0
      ast
    else
      eval_ast(ast, environment)
    end
  end

  def PRINT(input)
    pr_str(input, true)
  end

  def rep(input)
    PRINT(EVAL(READ(input), @repl_env))
  rescue InvalidHashmapKeyError => e
    "Error! Hashmap keys can only be strings or keywords."
  rescue NotCallableError => e
    e.message
  rescue SymbolNotFoundError => e
    e.message
  rescue UnbalancedEscapingError => e
    "Error! Detected unbalanced escaping. Check for matching '\\'."
  rescue UnbalancedHashmapError => e
    "Error! Detected unbalanced list. Check for matching '}'."
  rescue UnbalancedListError => e
    "Error! Detected unbalanced list. Check for matching ')'."
  rescue UnbalancedStringError => e
    "Error! Detected unbalanced string. Check for matching '\"'."
  rescue UnbalancedVectorError => e
    "Error! Detected unbalanced list. Check for matching ']'."
  end

  def eval_ast(mal, environment)
    case mal
    when Types::Symbol
      environment.get(mal)
    when Types::List
      list = Types::List.new
      mal.each { |i| list << EVAL(i, environment) }
      list
    when Types::Vector
      vec = Types::Vector.new
      mal.each { |i| vec << EVAL(i, environment) }
      vec
    when Types::Hashmap
      hashmap = Types::Hashmap.new
      mal.each { |k, v| hashmap[k] = EVAL(v, environment) }
      hashmap
    else
      mal
    end
  end
end

while input = Readline.readline("user> ")
  puts Mal.rep(input)
end

puts


