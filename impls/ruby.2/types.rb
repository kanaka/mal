module Mal
  module Types
    class Args < ::Array
    end

    class List < ::Array
      def meta
        @meta ||= Types::Nil.instance
      end

      def meta=(value)
        @meta = value
      end

      def to_list
        self
      end
    end

    class Vector < ::Array
      def meta
        @meta ||= Types::Nil.instance
      end

      def meta=(value)
        @meta = value
      end

      def to_list
        List.new(self)
      end
    end

    class Hashmap < ::Hash
      def meta
        @meta ||= Types::Nil.instance
      end

      def meta=(value)
        @meta = value
      end
    end

    class Base < ::Struct.new(:value)
      def inspect
        value.inspect
      end
    end

    class String < Base; end

    class Atom < Base
      def inspect
        "Atom<#{value.inspect}>"
      end
    end

    class Keyword < Base
      def self.for(value)
        @_keywords ||= {}

        if @_keywords.key?(value)
          @_keywords[value]
        else
          @_keywords[value] = new(value)
        end
      end
    end

    class Number < Base
      def +(other)
        self.class.new(value + other.value)
      end

      def -(other)
        self.class.new(value - other.value)
      end

      def *(other)
        self.class.new(value * other.value)
      end

      def /(other)
        self.class.new(value / other.value)
      end
    end

    class Symbol < Base
      def self.for(value)
        @_symbols ||= {}

        if @_symbols.key?(value)
          @_symbols[value]
        else
          @_symbols[value] = new(value)
        end
      end

      def inspect
        value
      end
    end

    class Nil < Base
      def self.instance
        @_instance ||= new(nil)
      end

      def inspect
        "nil"
      end
    end

    class True < Base
      def self.instance
        @_instance ||= new(true)
      end
    end

    class False < Base
      def self.instance
        @_instance ||= new(false)
      end
    end

    class Callable
      def initialize(&block)
        @fn = block
      end

      def call(args = nil)
        args = Types::Args.new if args.nil?
        raise unless args.is_a?(Types::Args)
        @fn.call(*args)
      end

      def inspect
        raise NotImplementedError, "invalid callable"
      end

      def is_mal_fn?
        false
      end

      def is_macro?
        false
      end

      def meta
        @meta ||= Types::Nil.instance
      end

      def meta=(value)
        @meta = value
      end
    end

    class Builtin < Callable
      attr_reader :name

      def initialize(name, &block)
        @name = name
        @fn = block
      end

      def inspect
        "#<builtin '#{name}'>"
      end
    end

    class Function < Callable
      attr_reader :ast, :params, :env

      def initialize(ast, params, env, &block)
        @ast = ast
        @params = params
        @env = env
        @fn = block
      end

      def inspect
        "#<function>"
      end

      def is_mal_fn?
        true
      end

      def to_macro
        Macro.new(ast, params, env, &@fn)
      end
    end

    class Macro < Callable
      attr_reader :ast, :params, :env

      def initialize(ast, params, env, &block)
        @ast = ast
        @params = params
        @env = env
        @fn = block
      end

      def inspect
        "#<macro>"
      end

      def is_mal_fn?
        true
      end

      def is_macro?
        true
      end
    end
  end
end
