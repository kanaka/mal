require "readline"

require_relative "errors"
require_relative "printer"
require_relative "reader"

module Mal
  extend self

  @repl_env = {
    '+' => -> (a, b) { a + b },
    '-' => -> (a, b) { a - b },
    '*' => -> (a, b) { a * b },
    '/' => -> (a, b) { a / b },
  }

  def READ(input)
    read_str(input)
  end

  def EVAL(ast, environment)
    if Types::List === ast && ast.size > 0
      evaluated = eval_ast(ast, environment)
      maybe_callable = evaluated.first

      if maybe_callable.respond_to?(:call)
        maybe_callable.call(*evaluated[1..])
      else
        raise NotCallableError, "Error! #{PRINT(maybe_callable)} is not callable."
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
      if @repl_env.key?(mal.value)
        @repl_env[mal.value]
      else
        raise SymbolNotFoundError, "Error! Symbol #{mal.value} not found."
      end
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


