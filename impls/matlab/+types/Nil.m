classdef Nil
    methods
        function len = length(obj)
            len = 0;
        end
        function ret = eq(a,b)
            ret = strcmp(class(b),'types.Nil');
        end
    end
end
