require "env"

class List < Array
end

class Vector < Array
end

def sequential?(obj)
    return obj.is_a?(List) || obj.is_a?(Vector)
end

class Function < Proc
    attr_accessor :ast
    attr_accessor :env
    attr_accessor :params

    def initialize(ast=nil, env=nil, params=nil, &block)
        super()
        @ast = ast
        @env = env
        @params = params
    end

    def gen_env(args)
        return Env.new(@env, @params, args)
    end
end
