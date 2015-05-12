require "./types"
require "./error"

module Mal

  class Env
    property data

    def initialize(@outer)
      @data = {} of String => Mal::Type
    end

    def initialize(@outer, binds, exprs : Array(Mal::Type))
      @data = {} of String => Mal::Type

      eval_error "binds must be list or vector" unless binds.is_a? Array

      varargs = false

      # Note:
      # Array#zip() can't be used because overload resolution failed
      (0...binds.size).each do |idx|
        sym, expr = binds[idx].unwrap, exprs[idx]
        eval_error "bind list must be symbol" unless sym.is_a? Mal::Symbol

        if sym.str == "&"
          varargs = true
          next
        end

        if varargs
          @data[sym.str] = Mal::Type.new exprs[idx-1..-1].each_with_object(Mal::List.new){|i, l| l << i}
          break
        end

        @data[sym.str] = expr
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
      eval_error "#{key} not found" unless e
      e.data[key]
    end
  end

end
