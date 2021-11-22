require_relative "errors"
require_relative "types"

module Mal
  class Env
    def initialize(outer = nil)
      @outer = outer
      @data = {}
    end

    def set(k, v)
      @data[k] = v
    end

    def find(k)
      if @data.key?(k)
        self
      elsif !@outer.nil?
        @outer.find(k)
      else
        Types::Nil.instance
      end
    end

    def get(k)
      environment = find(k)

      case environment
      when self.class
        environment.get_value(k)
      when Types::Nil
        raise SymbolNotFoundError, "Error! Symbol #{k.value} not found."
      end
    end

    def get_value(k)
      @data[k]
    end
  end
end
