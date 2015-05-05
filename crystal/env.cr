require "./types"

module Mal

  class Env
    property data

    def initialize(@outer)
      @data = {} of String => Mal::Type
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
