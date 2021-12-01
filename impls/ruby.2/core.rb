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

        Types::Symbol.for("prn") => Types::Builtin.new do |mal|
          val =
            if mal.any?
              mal.first
            else
              Types::Nil.instance
            end

          puts Mal.pr_str(val, true)

          Types::Nil.instance
        end,

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
        end
      }
    end
  end
end
