classdef HashMap < handle
    properties
        data = containers.Map();
        meta = types.nil;
    end
    methods
        function obj = HashMap(varargin)
            if nargin == 0
                obj.data = containers.Map();
            else
                obj.data = containers.Map(varargin(1:2:end), ...
                                          varargin(2:2:end));
            end
        end

        function len = length(obj)
            len = length(obj.data);
        end

        function ret = get(obj, key)
            ret = obj.data(key);
        end

        function ret = set(obj, key, val)
            obj.data(key) = val;
            ret = val;
        end

        function ret = keys(obj)
            ret = obj.data.keys();
        end

        function ret = values(obj)
            ret = obj.data.values();
        end

        function ret = clone(obj)
            ret = types.HashMap();
            if length(obj) > 0
                ret.data = containers.Map(obj.data.keys(), obj.data.values());
            else
                ret.data = containers.Map();
            end
            ret.meta = obj.meta;
        end
    end
end
