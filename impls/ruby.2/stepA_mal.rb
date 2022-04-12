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

    @repl_env.set(
      Types::Symbol.for("eval"),

      Types::Builtin.new("eval") do |mal|
        Mal.EVAL(mal, @repl_env)
      end
    )

    Mal.rep("(def! not (fn* (a) (if a false true)))")
    Mal.rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
    Mal.rep("(def! *host-language* \"ruby.2\")")
    Mal.rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

    if !run_application?
      Mal.rep("(def! *ARGV* (list))")
      Mal.rep("(println (str \"Mal [\" \*host-language\* \"]\"))")
    end
  end

  def run_application?
    ARGV.any?
  end

  def run!
    args = ARGV[1..].map(&:inspect)

    if args.any?
      Mal.rep("(def! *ARGV* (list #{args.join(" ")}))")
    else
      Mal.rep("(def! *ARGV* (list))")
    end

    file = File.absolute_path(ARGV.first)

    Dir.chdir(File.dirname(file)) do
      Mal.rep("(load-file #{file.inspect})")
    end
  end

  def READ(input)
    read_str(input)
  end

  def EVAL(ast, environment)
    loop do

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
          return environment.set(sym, EVAL(val, environment))
        when Types::Symbol.for("defmacro!")
          _, sym, val = ast
          result = EVAL(val, environment)

          case result
          when Types::Function
            return environment.set(sym, result.to_macro)
          else
            raise TypeError, "defmacro! must be bound to a function"
          end
        when Types::Symbol.for("let*")
          e = Env.new(environment)
          _, bindings, val = ast
          bindings = bindings.dup # TODO note bugfix let bindings w/ TCO loop and destructive mutation (shift)

          unless Types::List === bindings || Types::Vector === bindings
            raise InvalidLetBindingsError, "let* bindings must be a list or vector"
          end

          until bindings.empty?
            k, v = bindings.shift(2)

            raise InvalidLetBindingsError, "Invalid let* bindings 'nil' key" if k.nil?
            v = Types::Nil.instance if v.nil?

            e.set(k, EVAL(v, e))
          end

          if !val.nil?
            # Continue loop
            ast = val
            environment = e
          else
            return Types::Nil.instance
          end
        when Types::Symbol.for("do")
          _, *values = ast

          if !values.nil? && values.any?
            values[0...-1].each do |v|
              EVAL(v, environment)
            end

            # Continue loop
            ast = values.last
          else
            return Types::Nil.instance
          end
        when Types::Symbol.for("if")
          _, condition, when_true, when_false = ast

          case EVAL(condition, environment)
          when Types::False.instance, Types::Nil.instance
            if !when_false.nil?
              # Continue loop
              ast = when_false
            else
              return Types::Nil.instance
            end
          else
            if !when_true.nil?
              # Continue loop
              ast = when_true
            else
              raise InvalidIfExpressionError, "No expression to evaluate when true"
            end
          end
        when Types::Symbol.for("fn*")
          _, binds, to_eval = ast

          return Types::Function.new(to_eval, binds, environment) do |*exprs|
            EVAL(to_eval, Env.new(environment, binds, exprs))
          end
        when Types::Symbol.for("quote")
          _, ret = ast
          return ret
        when Types::Symbol.for("quasiquote")
          _, ast_rest = ast
          ast = quasiquote(ast_rest)
        when Types::Symbol.for("try*")
          _, to_try, catch_list = ast

          begin
            return EVAL(to_try, environment)
          rescue => e
            raise e if catch_list.nil? || catch_list&.empty?
            raise SyntaxError, "try* missing proper catch*" unless catch_list&.first == Types::Symbol.for("catch*")

            _, exception_symbol, exception_handler = catch_list

            value =
              if e.is_a?(MalError)
                e.value
              else
                Types::String.new(e.message)
              end

            return EVAL(
              exception_handler,
              Env.new(
                environment,
                Types::List.new([exception_symbol]),
                Types::List.new([value])
              )
            )
          end
        else
          maybe_callable = EVAL(ast.first, environment)
          if !maybe_callable.respond_to?(:call)
            raise NotCallableError, "Error! #{PRINT(maybe_callable)} is not callable."
          end
          raw_args = ast[1..]
          if maybe_callable.is_macro?
            if raw_args.any?
              ast = maybe_callable.call(Types::Args.new(raw_args))
            else
              ast = maybe_callable.call
            end
            next
          end
          args = Types::List.new
          raw_args.each { |i| args << EVAL(i, environment) }
          if maybe_callable.is_mal_fn?
            # Continue loop
            ast = maybe_callable.ast
            environment = Env.new(
              maybe_callable.env,
              maybe_callable.params,
              args,
            )
          elsif args.any?
            return maybe_callable.call(Types::Args.new(args))
          else
            return maybe_callable.call(Types::Args.new)
          end
        end
      else
        return ast
      end
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
  rescue MalError => e
    "Error: #{pr_str(e.value, true)}"
  rescue Error, TypeError => e
    "#{e.class} -- #{e.message}"
  rescue SkipCommentError
    nil
  end

  def quasiquote_list(mal)
    result = Types::List.new

    mal.reverse_each do |elt|
      if elt.is_a?(Types::List) && elt.first == Types::Symbol.for("splice-unquote")
        result = Types::List.new([
          Types::Symbol.for("concat"),
          elt[1],
          result
        ])
      else
        result = Types::List.new([
          Types::Symbol.for("cons"),
          quasiquote(elt),
          result
        ])
      end
    end

    result
  end

  def quasiquote(mal)
    case mal
    when Types::List
      if mal.first == Types::Symbol.for("unquote")
        mal[1]
      else
        quasiquote_list(mal)
      end
    when Types::Vector
      Types::List.new([
        Types::Symbol.for("vec"),
        quasiquote_list(mal)
      ])
    when Types::Hashmap, Types::Symbol
      Types::List.new([
        Types::Symbol.for("quote"),
        mal
      ])
    else
      mal
    end
  end

end

Mal.boot_repl!

if Mal.run_application?
  Mal.run!
else
  while input = Readline.readline("user> ")
    val = Mal.rep(input)
    puts val unless val.nil?
  end

  puts
end
