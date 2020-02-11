classdef Symbol
    properties
        name
    end
    methods
        function sym = Symbol(name)
            sym.name = name;
        end
        function ret = eq(a,b)
            ret = strcmp(a.name, b.name);
        end
    end
end
