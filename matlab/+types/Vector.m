classdef Vector < types.List
    methods
        function obj = Vector(varargin)
            obj.data = varargin;
        end

        function ret = slice(obj, start, last)
            if nargin < 3
                last = length(obj.data);
            end
            ret = types.Vector(obj.data{2:end});
        end

    end
end
