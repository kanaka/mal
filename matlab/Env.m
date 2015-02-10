classdef Env < handle
    properties
        data
        outer
    end
    methods
        function env = Env(outer, binds, exprs)
            env.data = containers.Map();
            env.outer = outer;

            if nargin > 1
                env = Env(outer);
                for i=1:length(binds)
                    k = binds.get(i).name;
                    if strcmp(k, '&')
                        env.data(binds.get(i+1).name) = exprs.slice(i);
                        break;
                    else
                        env.data(k) = exprs.get(i);
                    end
                end
            end
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
                                 sprintf('''%s'' not found', k.name)));
            end
        end
    end
end
