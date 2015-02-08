classdef Env < handle
    properties
        data
        outer
    end
    methods
        function env = Env(outer)
            env.data = containers.Map();
            env.outer = outer;
        end
        function ret = set(env, k, v)
            env.data(k.name) = v;
            ret = v;
        end
        function ret = find(env, k)
            if env.data.isKey(k.name)
                ret = env;
            else
                if ~islogical(env.outer) 
                    ret = env.outer.find(k);
                else
                    ret = false;
                end
            end
        end
        function ret = get(env, k)
            fenv = env.find(k);
            if ~islogical(fenv)
                ret = fenv.data(k.name);
            else
                throw(MException('ENV:notfound', ...
                                 strcat('''', k.name, ''' not found')));
            end
        end
    end
end
