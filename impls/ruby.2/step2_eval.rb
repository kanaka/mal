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
    # puts "EVAL: #{pr_str(ast, true)}"


    case ast
    when Types::Symbol
      if @repl_env.key?(ast.value)
        @repl_env[ast.value]
      else
        raise SymbolNotFoundError, "Error! Symbol #{ast.value} not found."
      end
    when Types::Vector
      vec = Types::Vector.new
      ast.each { |i| vec << EVAL(i, environment) }
      return vec
    when Types::Hashmap
      hashmap = Types::Hashmap.new
      ast.each { |k, v| hashmap[k] = EVAL(v, environment) }
      return hashmap
    when Types::List
      if ast.size == 0
        return ast
      end

      evaluated = Types::List.new
      ast.each { |i| evaluated << EVAL(i, environment) }
      maybe_callable = evaluated.first

      if maybe_callable.respond_to?(:call)
        maybe_callable.call(*evaluated[1..])
      else
        raise NotCallableError, "Error! #{PRINT(maybe_callable)} is not callable."
      end
    else
      return ast
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

end

while input = Readline.readline("user> ")
  puts Mal.rep(input)
end

puts


