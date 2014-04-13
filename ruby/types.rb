require "env"

class MalException < StandardError
  attr_reader :data
  def initialize(data)
    @data = data
  end
end

class List < Array
    attr_accessor :meta
    def conj(xs)
        xs.each{|x| self.unshift(x)}
        return self
    end
end

class Vector < Array
    attr_accessor :meta
    def conj(xs)
        self.push(*xs)
        return self
    end
end

class Hash # re-open and add meta
    attr_accessor :meta
end

def sequential?(obj)
    return obj.is_a?(List) || obj.is_a?(Vector)
end

class Proc # re-open and add meta
    attr_accessor :meta
end

class Function < Proc
    attr_accessor :ast
    attr_accessor :env
    attr_accessor :params
    attr_accessor :is_macro

    def initialize(ast=nil, env=nil, params=nil, &block)
        super()
        @ast = ast
        @env = env
        @params = params
        @is_macro = false
    end

    def gen_env(args)
        return Env.new(@env, @params, args)
    end
end

class Atom
    attr_accessor :meta
    attr_accessor :val
    def initialize(val)
        @val = val
    end
end
