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

    def get(k)
      if @data.key?(k)
        @data[k]
      elsif !@outer.nil?
        @outer.get(k)
      else
        0
      end
    end
  end
end
