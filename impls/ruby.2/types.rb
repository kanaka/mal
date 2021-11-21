module Mal
  module Types
    class List < ::Array; end
    class Vector < ::Array; end
    class Hashmap < ::Hash; end

    class Atom < ::Struct.new(:value); end
    class Keyword < Atom; end

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
    class Symbol < Atom; end

    class Nil < Atom
      def initialize(_value)
        @value = nil
      end
    end

    class True < Atom
      def initialize(_value)
        @value = true
      end
    end

    class False < Atom
      def initialize(_value)
        @value = false
      end
    end
  end
end
