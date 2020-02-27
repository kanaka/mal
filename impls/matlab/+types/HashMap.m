classdef HashMap < handle
    properties
        data
        meta = type_utils.nil;
    end
    methods
        function obj = HashMap(varargin)
            if nargin == 0
                if exist('OCTAVE_VERSION', 'builtin') ~= 0
                    obj.data = Dict();
                else
                    obj.data = containers.Map();
                end
            else
                if exist('OCTAVE_VERSION', 'builtin') ~= 0
                    obj.data = Dict();
                    for i=1:2:length(varargin)
                        obj.data(varargin{i}) = varargin{i+1};
                    end
                else
                    obj.data = containers.Map(varargin(1:2:end), ...
                                              varargin(2:2:end));
                end
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
                if exist('OCTAVE_VERSION', 'builtin') ~= 0
                    ret.data = Dict(obj.data.keys(), obj.data.values());
                else
                    ret.data = containers.Map(obj.data.keys(), obj.data.values());
                end
            else
                if exist('OCTAVE_VERSION', 'builtin') ~= 0
                    ret.data = Dict();
                else
                    ret.data = containers.Map();
                end
            end
            ret.meta = obj.meta;
        end
    end
end
