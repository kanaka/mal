classdef List < handle
    properties
        data
        meta = type_utils.nil;
    end
    methods
        function obj = List(varargin)
            obj.data = varargin;
            meta = type_utils.nil;
        end

        function len = length(obj)
            len = length(obj.data);
        end

        function ret = get(obj, idx)
            ret = obj.data{idx};
        end

        function ret = set(obj, key, val)
            obj.data{key} = val;
            ret = val;
        end

        function ret = append(obj, val)
            obj.data{end+1} = val;
            ret = val;
        end

        function ret = slice(obj, start, last)
            if nargin < 3
                last = length(obj.data);
            end
            ret = types.List(obj.data{start:last});
        end

        function ret = clone(obj)
            ret = types.List();
            ret.data = obj.data;
            ret.meta = obj.meta;
        end

%        function varargout = subsref(vec, S)
%            % This doesn't work for ranges
%            [varargout{1:nargout}] = builtin('subsref', vec.data, S);
%
%            varargout = cell(1,max(1,nargout));
%            [varargout{:}] = builtin('subsref',vec.data,S);
%
%%            switch S.type
%%            case '()'
%%                varargout = cell(1,numel(vec));
%%                varargout{1} = builtin('subsref', vec.data, S);
%%            case '{}'
%%                varargout = cell(1,numel(vec));
%%                varargout{1} = builtin('subsref', vec.data, S);
%%            case '.'
%%                error('Vector property access not yet implemented');
%%            end
%        end

%        %function n = numel(varargin)
%        %    n = 1;
%        %end

    end
end
