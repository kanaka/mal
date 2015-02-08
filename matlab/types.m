classdef types
    properties (Constant = true)
        nil = types.Nil();
    end

    methods(Static)
        function ret = equal(a,b)
            ret = false;
            ota = class(a); otb = class(b);
            if ~(strcmp(ota,otb) || (iscell(a) && iscell(b)))
                return;
            end
            switch (ota)
            case 'cell'
                if ~(length(a) == length(b))
                    return
                end
                for i=1:length(a)
                    if ~(types.equal(a{i}, b{i}))
                        return
                    end
                end
                ret = true;
            case 'char'
                ret = strcmp(a,b);
            otherwise
                ret = a == b;
            end
        end
    end
end

