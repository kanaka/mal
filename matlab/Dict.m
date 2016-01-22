% Implement containers.Map like structure
% This only applies to GNU Octave and will break in Matlab when
% arbitrary string keys are used.
classdef Dict < handle
    properties
        data
    end
    methods
        function dict = Dict(keys, values)
            dict.data = struct();

            if nargin > 0
                for i=1:length(keys)
                    dict.data.(keys{i}) = values{i};
                end
            end
        end

        function ret = subsasgn(dict, ind, val)
            dict.data.(ind(1).subs{1}) = val;
            ret = dict;
        end
        function ret = subsref(dict, ind)
            if strcmp('.', ind(1).type)
                % Function call
                switch ind(1).subs
                case 'isKey'
                    if numel(ind) > 1
                        ret = isfield(dict.data, ind(2).subs{1});
                    else
                        error('Dict:invalidArgs', ...
                            sprintf('''%s'' called with no arguments', ind(1).subs));
                    end
                case 'keys'
                    ret = fieldnames(dict.data);
                case 'values'
                    ret = {};
                    keys = fieldnames(dict.data);
                    for i=1:length(keys)
                        ret{end+1} = dict.data.(keys{i});
                    end
                case 'remove'
                    if numel(ind) > 1
                        if numel(ind(2).subs) > 0
                            dict.data = rmfield(dict.data, ind(2).subs{1});
                        end
                    else
                        error('Dict:invalidArgs', ...
                            sprintf('''%s'' called with no arguments', ind(1).subs));
                    end
                otherwise
                    error('Dict:notfound', ...
                          sprintf('''%s'' not found', ind(1).subs));
                end
            else
                % Key lookup
                ret = dict.data.(ind(1).subs{1});
            end
        end
    end
end
