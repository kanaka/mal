require "readline"

require_relative "types"

module Mal
  module Core
    extend self

    def ns
      {
        Types::Symbol.for("+") => Types::Builtin.new("+") do |a, b|
          a + b
        end,

        Types::Symbol.for("-") => Types::Builtin.new("-") { |a, b| a - b },
        Types::Symbol.for("*") => Types::Builtin.new("*") { |a, b| a * b },
        Types::Symbol.for("/") => Types::Builtin.new("/") { |a, b| a / b },

        Types::Symbol.for("list") => Types::Builtin.new("list") do |*mal|
          list = Types::List.new
          mal.each { |m| list << m }
          list
        end,

        Types::Symbol.for("list?") => Types::Builtin.new("list?") do |list = nil|
          list.is_a?(Types::List) ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("vector?") => Types::Builtin.new("vector?") do |vector = nil|
          vector.is_a?(Types::Vector) ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("string?") => Types::Builtin.new("string?") do |string = nil|
          string.is_a?(Types::String) ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("number?") => Types::Builtin.new("number?") do |number = nil|
          number.is_a?(Types::Number) ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("fn?") => Types::Builtin.new("fn?") do |fn = nil|
          fn.is_a?(Types::Callable) && !fn.is_macro? ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("macro?") => Types::Builtin.new("macro?") do |macro = nil|
          macro.is_a?(Types::Callable) && macro.is_macro? ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("empty?") => Types::Builtin.new("empty?") do |list_or_vector = nil|
          is_empty =
            case list_or_vector
            when Types::List, Types::Vector
              list_or_vector.empty?
            else
              true
            end

          is_empty ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("count") => Types::Builtin.new("count") do |*mal|
          count =
            if mal.any?
              case mal.first
              when Types::List, Types::Vector
                mal.first.size
              else
                0
              end
            else
              0
            end

          Types::Number.new(count)
        end,

        Types::Symbol.for("=") => Types::Builtin.new("=") do |a, b|
          if a.nil? || b.nil?
            Types::False.instance
          else
            if a == b
              Types::True.instance
            else
              Types::False.instance
            end
          end
        end,

        Types::Symbol.for("<") => Types::Builtin.new("<") do |a, b|
          if a.nil? || b.nil?
            Types::False.instance
          else
            if a.is_a?(Types::Number) && b.is_a?(Types::Number)
              if a.value < b.value
                Types::True.instance
              else
                Types::False.instance
              end
            else
              Types::False.instance
            end
          end
        end,

        Types::Symbol.for("<=") => Types::Builtin.new("<=") do |a, b|
          if a.nil? || b.nil?
            Types::False.instance
          else
            if a.is_a?(Types::Number) && b.is_a?(Types::Number)
              if a.value <= b.value
                Types::True.instance
              else
                Types::False.instance
              end
            else
              Types::False.instance
            end
          end
        end,

        Types::Symbol.for(">") => Types::Builtin.new(">") do |a, b|
          if a.nil? || b.nil?
            Types::False.instance
          else
            if a.is_a?(Types::Number) && b.is_a?(Types::Number)
              if a.value > b.value
                Types::True.instance
              else
                Types::False.instance
              end
            else
              Types::False.instance
            end
          end
        end,

        Types::Symbol.for(">=") => Types::Builtin.new(">=") do |a, b|
          if a.nil? || b.nil?
            Types::False.instance
          else
            if a.is_a?(Types::Number) && b.is_a?(Types::Number)
              if a.value >= b.value
                Types::True.instance
              else
                Types::False.instance
              end
            else
              Types::False.instance
            end
          end
        end,

        Types::Symbol.for("pr-str") => Types::Builtin.new("pr-str") do |*mal|
          Types::String.new(mal.map { |m| Mal.pr_str(m, true) }.join(" "))
        end,

        Types::Symbol.for("str") => Types::Builtin.new("str") do |*mal|
          Types::String.new(mal.map { |m| Mal.pr_str(m, false) }.join(""))
        end,

        Types::Symbol.for("prn") => Types::Builtin.new("prn") do |*mal|
          puts mal.map { |m| Mal.pr_str(m, true) }.join(" ")
          Types::Nil.instance
        end,

        Types::Symbol.for("println") => Types::Builtin.new("println") do |*mal|
          puts mal.map { |m| Mal.pr_str(m, false) }.join(" ")
          Types::Nil.instance
        end,

        Types::Symbol.for("read-string") => Types::Builtin.new("read-string") do |string = nil|
          if string.is_a?(Types::String)
            Mal.read_str(string.value)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("slurp") => Types::Builtin.new("slurp") do |file = nil|
          if file.is_a?(Types::String)
            if File.exist?(file.value)
              Types::String.new(File.read(file.value))
            else
              raise FileNotFoundError, file.value
            end
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("atom") => Types::Builtin.new("atom") do |mal|
          Types::Atom.new(mal)
        end,

        Types::Symbol.for("atom?") => Types::Builtin.new("atom?") do |maybe_atom|
          maybe_atom.is_a?(Types::Atom) ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("deref") => Types::Builtin.new("deref") do |maybe_atom|
          maybe_atom.is_a?(Types::Atom) ? maybe_atom.value : Types::Nil.instance
        end,

        Types::Symbol.for("reset!") => Types::Builtin.new("reset!") do |atom, value|
          if value.nil?
            value = Types::Nil.instance
          end

          atom.value = value
        end,

        Types::Symbol.for("swap!") => Types::Builtin.new("swap!") do |atom, fn, *args|
          atom.value = fn.call(Types::Args.new([atom.value, *args]))
        end,

        Types::Symbol.for("cons") => Types::Builtin.new("cons") do |val, list_or_vector|
          Types::List.new([val, *list_or_vector])
        end,

        Types::Symbol.for("concat") => Types::Builtin.new("concat") do |*mal|
          list = Types::List.new

          mal.each do |l|
            list.concat(l)
          end

          list
        end,

        Types::Symbol.for("vec") => Types::Builtin.new("vec") do |list_or_vector|
          case list_or_vector
          when Types::List
            vec = Types::Vector.new

            list_or_vector.each do |m|
              vec << m
            end

            vec
          when Types::Vector
            list_or_vector
          else
            raise TypeError, "invalid `vec` arguments, must be vector or list"
          end
        end,

        Types::Symbol.for("nth") => Types::Builtin.new("nth") do |list_or_vector, index|
          result = list_or_vector[index.value]
          raise IndexError, "Index #{index.value} is out of bounds" if result.nil?
          result
        end,

        Types::Symbol.for("first") => Types::Builtin.new("first") do |list_or_vector|
          if !list_or_vector.nil? && list_or_vector != Types::Nil.instance
            result = list_or_vector.first

            if result.nil?
              result = Types::Nil.instance
            end

            result
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("rest") => Types::Builtin.new("rest") do |list_or_vector|
          if !list_or_vector.nil? && list_or_vector != Types::Nil.instance
            result = list_or_vector[1..]

            if result.nil?
              result = Types::List.new
            end

            result.to_list
          else
            Types::List.new
          end
        end,

        Types::Symbol.for("throw") => Types::Builtin.new("throw") do |to_throw|
          raise MalError, to_throw
        end,

        Types::Symbol.for("apply") => Types::Builtin.new("apply") do |fn, *rest|
          args = Types::Args.new

          rest.flatten(1).each do |a|
            args << a
          end

          fn.call(args)
        end,

        Types::Symbol.for("map") => Types::Builtin.new("map") do |fn, *rest|
          results = Types::List.new

          rest.flatten(1).each do |a|
            results << fn.call(Types::Args.new([a]))
          end

          results
        end,

        Types::Symbol.for("nil?") => Types::Builtin.new("nil?") do |mal|
          if mal == Types::Nil.instance
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("true?") => Types::Builtin.new("true?") do |mal|
          if mal == Types::True.instance
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("false?") => Types::Builtin.new("false?") do |mal|
          if mal == Types::False.instance
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("symbol?") => Types::Builtin.new("symbol?") do |mal|
          if mal.is_a?(Types::Symbol)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("keyword?") => Types::Builtin.new("keyword?") do |mal|
          if mal.is_a?(Types::Keyword)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("symbol") => Types::Builtin.new("symbol") do |string|
          if string
            Types::Symbol.for(string.value)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("keyword") => Types::Builtin.new("keyword") do |keyword|
          if keyword
            Types::Keyword.for(keyword.value)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("vector") => Types::Builtin.new("vector") do |*items|
          vector = Types::Vector.new

          items.each do |i|
            vector << i
          end

          vector
        end,

        Types::Symbol.for("sequential?") => Types::Builtin.new("sequential?") do |list_or_vector|
          case list_or_vector
          when Types::List, Types::Vector
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("hash-map") => Types::Builtin.new("hash-map") do |*items|
          raise UnbalancedHashmapError, "unbalanced hashmap error, arguments must be even" if items&.size&.odd?

          hashmap = Types::Hashmap.new

          items.each_slice(2) do |(k, v)|
            hashmap[k] = v
          end

          hashmap
        end,

        Types::Symbol.for("map?") => Types::Builtin.new("map?") do |mal|
          if mal.is_a?(Types::Hashmap)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("assoc") => Types::Builtin.new("assoc") do |hashmap, *items|
          raise UnbalancedHashmapError, "unbalanced hashmap error, arguments must be even" if items.size&.odd?

          new_hashmap = hashmap.dup

          items.each_slice(2) do |(k, v)|
            new_hashmap[k] = v
          end

          new_hashmap
        end,

        Types::Symbol.for("dissoc") => Types::Builtin.new("dissoc") do |hashmap, *keys|
          new_hashmap = Types::Hashmap.new

          hashmap.keys.each do |k|
            next if keys.include?(k)
            new_hashmap[k] = hashmap[k]
          end

          new_hashmap
        end,

        Types::Symbol.for("get") => Types::Builtin.new("get") do |hashmap, key|
          if Types::Hashmap === hashmap && key && hashmap.key?(key)
            hashmap[key]
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("contains?") => Types::Builtin.new("contains?") do |hashmap, key|
          if Types::Hashmap === hashmap && key && hashmap.key?(key)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("keys") => Types::Builtin.new("keys") do |hashmap|
          if Types::Hashmap === hashmap
            Types::List.new(hashmap.keys)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("vals") => Types::Builtin.new("vals") do |hashmap|
          if Types::Hashmap === hashmap
            Types::List.new(hashmap.values)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("readline") => Types::Builtin.new("readline") do |prompt = nil|
          prompt =
            if prompt.nil?
              "user> "
            else
              prompt.value
            end

          input = Readline.readline(prompt)

          if input.nil?
            Types::Nil.instance
          else
            Types::String.new(input)
          end
        end,

        Types::Symbol.for("meta") => Types::Builtin.new("meta") do |value|
          case value
          when Types::List, Types::Vector, Types::Hashmap, Types::Callable
            value.meta
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("with-meta") => Types::Builtin.new("with-meta") do |value, meta|
          case value
          when Types::List, Types::Vector, Types::Hashmap, Types::Callable
            new_value = value.dup
            new_value.meta = meta
            new_value
          else
            raise TypeError, "Unable to use meta with #{Mal.pr_str(value)}"
          end
        end,

        Types::Symbol.for("time-ms") => Types::Builtin.new("time-ms") do
          Types::Number.new((Time.now.to_f.round(3) * 1000).to_i)
        end,

        Types::Symbol.for("conj") => Types::Builtin.new("conj") do |list_or_vector, *new_elems|
          case list_or_vector
          when Types::List
            Types::List.new([*new_elems.reverse, *list_or_vector])
          when Types::Vector
            Types::Vector.new([*list_or_vector, *new_elems])
          else
            raise TypeError, "Unable to `conj` with <#{Mal.pr_str(list_or_vector)}>, must be list or vector"
          end
        end,

        Types::Symbol.for("seq") => Types::Builtin.new("seq") do |sequential|
          case sequential
          when Types::List
            if sequential.any?
              sequential
            else
              Types::Nil.instance
            end
          when Types::Vector
            if sequential.any?
              Types::List.new(sequential)
            else
              Types::Nil.instance
            end
          when Types::String
            if !sequential.value.empty?
              Types::List.new(sequential.value.chars.map { |c| Types::String.new(c) })
            else
              Types::Nil.instance
            end
          when Types::Nil
            Types::Nil.instance
          else
            raise TypeError, "Unable to `seq` with <#{Mal.pr_str(sequential)}>, must be list, vector, string, or nil"
          end
        end
      }
    end
  end
end
