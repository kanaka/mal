require_relative "types"

module Mal
  module Core
    extend self

    def ns
      {
        Types::Symbol.for("+") => Types::Builtin.new do |a, b|
          a + b
        end,

        Types::Symbol.for("-") => Types::Builtin.new { |a, b| a - b },
        Types::Symbol.for("*") => Types::Builtin.new { |a, b| a * b },
        Types::Symbol.for("/") => Types::Builtin.new { |a, b| a / b },

        Types::Symbol.for("list") => Types::Builtin.new do |mal|
          list = Types::List.new
          mal.each { |m| list << m }
          list
        end,

        Types::Symbol.for("list?") => Types::Builtin.new do |mal|
          is_list =
            if mal.any?
              Types::List === mal.first
            else
              false
            end

          is_list ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("vector?") => Types::Builtin.new do |mal|
          is_vector =
            if mal.any?
              Types::Vector === mal.first
            else
              false
            end

          is_vector ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("empty?") => Types::Builtin.new do |mal|
          is_empty =
            if mal.any?
              case mal.first
              when Types::List, Types::Vector
                mal.first.empty?
              else
                Types::True.instance
              end
            else
              Types::True.instance
            end

          is_empty ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("count") => Types::Builtin.new do |mal|
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

        Types::Symbol.for("=") => Types::Builtin.new do |mal|
          a, b = mal

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

        Types::Symbol.for("<") => Types::Builtin.new do |mal|
          a, b = mal

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

        Types::Symbol.for("<=") => Types::Builtin.new do |mal|
          a, b = mal

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

        Types::Symbol.for(">") => Types::Builtin.new do |mal|
          a, b = mal

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

        Types::Symbol.for(">=") => Types::Builtin.new do |mal|
          a, b = mal

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

        Types::Symbol.for("pr-str") => Types::Builtin.new do |mal|
          Types::String.new(mal.map { |m| Mal.pr_str(m, true) }.join(" "))
        end,

        Types::Symbol.for("str") => Types::Builtin.new do |mal|
          Types::String.new(mal.map { |m| Mal.pr_str(m, false) }.join(""))
        end,

        Types::Symbol.for("prn") => Types::Builtin.new do |mal|
          puts mal.map { |m| Mal.pr_str(m, true) }.join(" ")
          Types::Nil.instance
        end,

        Types::Symbol.for("println") => Types::Builtin.new do |mal|
          puts mal.map { |m| Mal.pr_str(m, false) }.join(" ")
          Types::Nil.instance
        end,

        Types::Symbol.for("read-string") => Types::Builtin.new do |mal|
          if mal.first.is_a?(Types::String)
            Mal.read_str(mal.first.value)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("slurp") => Types::Builtin.new do |mal|
          if mal.first.is_a?(Types::String)
            if File.exist?(mal.first.value)
              Types::String.new(File.read(mal.first.value))
            else
              raise FileNotFoundError, mal.first.value
            end
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("atom") => Types::Builtin.new do |mal|
          Types::Atom.new(mal.first)
        end,

        Types::Symbol.for("atom?") => Types::Builtin.new do |mal|
          mal.first.is_a?(Types::Atom) ? Types::True.instance : Types::False.instance
        end,

        Types::Symbol.for("deref") => Types::Builtin.new do |mal|
          mal.first.is_a?(Types::Atom) ? mal.first.value : Types::Nil.instance
        end,

        Types::Symbol.for("reset!") => Types::Builtin.new do |mal|
          atom, value = mal

          if value.nil?
            value = Types::Nil.instance
          end

          atom.value = value
        end,

        Types::Symbol.for("swap!") => Types::Builtin.new do |mal|
          atom, fn, *args = mal
          atom.value = fn.call(Types::List.new([atom.value, *args]))
        end,

        Types::Symbol.for("cons") => Types::Builtin.new do |mal|
          val, list_or_vector = mal
          Types::List.new([val, *list_or_vector])
        end,

        Types::Symbol.for("concat") => Types::Builtin.new do |mal|
          list = Types::List.new

          mal.each do |l|
            list.concat(l)
          end

          list
        end,

        Types::Symbol.for("vec") => Types::Builtin.new do |mal|
          case mal.first
          when Types::List
            vec = Types::Vector.new

            mal.first.each do |m|
              vec << m
            end

            vec
          when Types::Vector
            mal.first
          else
            raise TypeError
          end
        end,

        Types::Symbol.for("nth") => Types::Builtin.new do |mal|
          list_or_vector, index = mal
          result = list_or_vector[index.value]
          raise IndexError, "Index #{index.value} is out of bounds" if result.nil?
          result
        end,

        Types::Symbol.for("first") => Types::Builtin.new do |mal|
          list_or_vector, * = mal

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

        Types::Symbol.for("rest") => Types::Builtin.new do |mal|
          list_or_vector, * = mal

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

        Types::Symbol.for("throw") => Types::Builtin.new do |mal|
          to_throw, * = mal
          raise MalError, to_throw
        end,

        Types::Symbol.for("apply") => Types::Builtin.new do |mal|
          fn, *rest = mal

          args = Types::List.new

          rest.flatten(1).each do |a|
            args << a
          end

          fn.call(args)
        end,

        Types::Symbol.for("map") => Types::Builtin.new do |mal|
          fn, rest = mal

          map_with =
            case rest
            when Types::List, Types::Vector
              rest
            else
              raise SyntaxError, "Must pass list/vector to map!"
            end

          results = Types::List.new

          map_with.each do |a|
            results << fn.call(a)
          end

          results
        end,

        Types::Symbol.for("nil?") => Types::Builtin.new do |mal|
          if mal&.first == Types::Nil.instance
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("true?") => Types::Builtin.new do |mal|
          if mal&.first == Types::True.instance
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("false?") => Types::Builtin.new do |mal|
          if mal&.first == Types::False.instance
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("symbol?") => Types::Builtin.new do |mal|
          if mal&.first&.is_a?(Types::Symbol)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("keyword?") => Types::Builtin.new do |mal|
          if mal&.first&.is_a?(Types::Keyword)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("symbol") => Types::Builtin.new do |mal|
          string, * = mal
          if string
            Types::Symbol.for(string.value)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("keyword") => Types::Builtin.new do |mal|
          string, * = mal
          if string
            Types::Keyword.for(string.value)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("vector") => Types::Builtin.new do |mal|
          *items = mal

          vector = Types::Vector.new

          items.each do |i|
            vector << i
          end

          vector
        end,

        Types::Symbol.for("sequential?") => Types::Builtin.new do |mal|
          list_or_vector, * = mal

          case list_or_vector
          when Types::List, Types::Vector
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("hash-map") => Types::Builtin.new do |mal|
          *items = mal

          raise UnbalancedHashmapError if items&.size&.odd?

          hashmap = Types::Hashmap.new

          items.each_slice(2) do |(k, v)|
            hashmap[k] = v
          end

          hashmap
        end,

        Types::Symbol.for("map?") => Types::Builtin.new do |mal|
          if mal&.first&.is_a?(Types::Hashmap)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("assoc") => Types::Builtin.new do |mal|
          hashmap, *items = mal

          raise UnbalancedHashmapError if items&.size&.odd?

          new_hashmap = hashmap.dup

          items.each_slice(2) do |(k, v)|
            new_hashmap[k] = v
          end

          new_hashmap
        end,

        Types::Symbol.for("dissoc") => Types::Builtin.new do |mal|
          hashmap, *keys = mal

          new_hashmap = Types::Hashmap.new

          hashmap.keys.each do |k|
            next if keys.include?(k)
            new_hashmap[k] = hashmap[k]
          end

          new_hashmap
        end,

        Types::Symbol.for("get") => Types::Builtin.new do |mal|
          hashmap, key = mal

          if Types::Hashmap === hashmap && key && hashmap.key?(key)
            hashmap[key]
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("contains?") => Types::Builtin.new do |mal|
          hashmap, key = mal

          if Types::Hashmap === hashmap && key && hashmap.key?(key)
            Types::True.instance
          else
            Types::False.instance
          end
        end,

        Types::Symbol.for("keys") => Types::Builtin.new do |mal|
          hashmap, * = mal

          if Types::Hashmap === hashmap
            Types::List.new(hashmap.keys)
          else
            Types::Nil.instance
          end
        end,

        Types::Symbol.for("vals") => Types::Builtin.new do |mal|
          hashmap, * = mal

          if Types::Hashmap === hashmap
            Types::List.new(hashmap.values)
          else
            Types::Nil.instance
          end
        end
      }
    end
  end
end
