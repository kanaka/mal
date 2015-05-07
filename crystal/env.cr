require "./types"

module Mal

  class Env
    property data

    def initialize(@outer)
      @data = {} of String => Mal::Type
    end

    def initialize(@outer, binds, exprs : Array(Mal::Type))
      @data = {} of String => Mal::Type

      raise EvalException.new "binds must be list or vector" unless binds.is_a?(Array)

      # Note:
      # Array#zip() can't be used because overload resolution failed
      (0..binds.size).each do |idx|
        sym, expr = binds[idx], exprs[idx]
        raise EvalException.new "bind list must be symbol" unless sym.is_a?(Mal::Symbol)
        @data[sym.val] = expr
      end
    end

    def set(key, value)
      @data[key] = value
    end

    def find(key)
      return self if @data.has_key? key

      o = @outer
      if o
        o.find key
      else
        nil
      end
    end

    def get(key)
      e = find(key)
      raise EvalException.new "#{key} not found" unless e
      e.data[key]
    end
  end

end
