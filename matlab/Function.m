classdef Function < handle
    properties
        fn
        ast
        env
        params
    end
    methods
        function f = Function(fn, ast, env, params)
            f.fn = fn;
            f.ast = ast;
            f.env = env;
            f.params = params;
        end
    end
end
