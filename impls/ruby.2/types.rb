module Mal
  module Types
    class List < ::Array; end
    class Vector < ::Array; end
    class Hashmap < ::Hash; end

    class Atom < ::Struct.new(:value); end

    class Keyword < Atom
      def self.for(value)
        @_keywords ||= {}

        if @_keywords.key?(value)
          @_keywords[value]
        else
          @_keywords[value] = new(value)
        end
      end
    end

    class Number < Atom
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

    class String < Atom; end

    class Symbol < Atom
      def self.for(value)
        @_symbols ||= {}

        if @_symbols.key?(value)
          @_symbols[value]
        else
          @_symbols[value] = new(value)
        end
      end
    end

    class Nil < Atom
      def self.instance
        @_instance ||= new
      end

      def initialize
        @value = nil
      end
    end

    class True < Atom
      def self.instance
        @_instance ||= new
      end

      def initialize
        @value = true
      end
    end

    class False < Atom
      def self.instance
        @_instance ||= new
      end

      def initialize
        @value = false
      end
    end
  end
end
