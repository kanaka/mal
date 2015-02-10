classdef Function < handle
    properties
        fn
        ast
        env
        params
        is_macro = false
        meta = types.nil;
    end
    methods
        function f = Function(fn, ast, env, params)
            f.fn = fn;
            f.ast = ast;
            f.env = env;
            f.params = params;
        end

        function ret = clone(obj)
            ret = types.Function(obj.fn, obj.ast, obj.env, obj.params);
            ret.is_macro = obj.is_macro;
            ret.meta = obj.meta;
        end
    end
end
