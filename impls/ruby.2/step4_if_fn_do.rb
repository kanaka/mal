require "readline"

require_relative "core"
require_relative "env"
require_relative "errors"
require_relative "printer"
require_relative "reader"

module Mal
  extend self

  def boot_repl!
    @repl_env = Env.new

    Core.ns.each do |k, v|
      @repl_env.set(k, v)
    end

    Mal.rep("(def! not (fn* (a) (if a false true)))")
  end

  def READ(input)
    read_str(input)
  end

  def EVAL(ast, environment)
    case environment.get(Types::Symbol.for("DEBUG-EVAL"))
    when 0, Types::Nil, Types::False
    else
      puts "EVAL: #{pr_str(ast, true)}"
    end

    case ast
    when Types::Symbol
      value = environment.get(ast)
      if value == 0
        raise SymbolNotFoundError, "'#{ast.value}' not found"
      end
      return value
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
      when Types::Symbol.for("do")
        _, *values = ast

        if !values.nil?
          evaluated = Types::List.new

          values.each do |v|
            evaluated << EVAL(v, environment)
          end

          evaluated.last
        else
          Types::Nil.instance
        end
      when Types::Symbol.for("if")
        _, condition, when_true, when_false = ast

        case EVAL(condition, environment)
        when Types::False.instance, Types::Nil.instance
          if !when_false.nil?
            EVAL(when_false, environment)
          else
            Types::Nil.instance
          end
        else
          if !when_true.nil?
            EVAL(when_true, environment)
          else
            raise InvalidIfExpressionError
          end
        end
      when Types::Symbol.for("fn*")
        _, binds, to_eval = ast

        Types::Function.new(to_eval, binds, environment) do |*exprs|
          EVAL(to_eval, Env.new(environment, binds, exprs))
        end
      else
        evaluated = Types::List.new
        ast.each { |i| evaluated << EVAL(i, environment) }
        maybe_callable = evaluated.first

        if maybe_callable.respond_to?(:call)
          maybe_callable.call(Types::Args.new(evaluated[1..]))
        else
          raise NotCallableError, "Error! #{PRINT(maybe_callable)} is not callable."
        end
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

Mal.boot_repl!

while input = Readline.readline("user> ")
  puts Mal.rep(input)
end

puts


