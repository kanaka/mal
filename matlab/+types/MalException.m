classdef MalException < MException
    properties
        obj
    end
    methods
        function exc = MalException(obj)
            exc@MException('MalException:object', 'MalException'); 
            exc.obj = obj;
        end
    end
end
