require_relative "errors"
require_relative "types"

module Mal
  class Env
    def initialize(outer = nil, binds = Types::List.new, exprs = Types::List.new)
      @outer = outer
      @data = {}

      spread_next = false
      binds.each_with_index do |b, i|
        if b.value == "&"
          spread_next = true
        else
          if spread_next
            set(b, Types::List.new(exprs[(i - 1)..]) || Types::Nil.instance)
            break
          else
            set(b, exprs[i] || Types::Nil.instance)
          end
        end
      end
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
        raise SymbolNotFoundError, "'#{k.value}' not found"
      end
    end

    def get_value(k)
      @data[k]
    end
  end
end
