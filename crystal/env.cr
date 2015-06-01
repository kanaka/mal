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

      # Note:
      # Array#zip() can't be used because overload resolution failed
      (0...binds.size).each do |idx|
        sym = binds[idx].unwrap
        eval_error "bind name must be symbol" unless sym.is_a? Mal::Symbol

        if sym.str == "&"
          eval_error "missing variable parameter name" if binds.size == idx
          next_param = binds[idx+1].unwrap
          eval_error "bind name must be symbol" unless next_param.is_a? Mal::Symbol
          var_args = Mal::List.new
          exprs[idx..-1].each{|e| var_args << e} if idx < exprs.size
          @data[next_param.str] = Mal::Type.new var_args
          break
        end

        @data[sym.str] = exprs[idx]
      end
    end

    def dump
      puts "ENV BEGIN".colorize.red
      @data.each do |k, v|
        puts "  #{k} -> #{print(v)}".colorize.red
      end
      puts "ENV END".colorize.red
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
      e = find key
      eval_error "'#{key}' not found" unless e
      e.data[key]
    end
  end

end
