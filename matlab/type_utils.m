classdef type_utils
    properties (Constant = true)
        nil = types.Nil();
    end

    methods(Static)
        function ret = equal(a,b)
            ret = false;
            ota = class(a); otb = class(b);
            if ~(strcmp(ota,otb) || ...
               (type_utils.sequential_Q(a) && type_utils.sequential_Q(b)))
                return;
            end
            switch (ota)
            case {'types.List', 'types.Vector'}
                if ~(length(a) == length(b))
                    return;
                end
                for i=1:length(a)
                    if ~(type_utils.equal(a.get(i), b.get(i)))
                        return;
                    end
                end
                ret = true;
            case 'types.HashMap'
                if ~(length(a) == length(b))
                    return;
                end
                ks1 = a.keys();
                for i=1:length(ks1)
                    k = ks1{i};
                    if ~(b.data.isKey(k))
                        return;
                    end
                    if ~(type_utils.equal(a.data(k), b.data(k)))
                        return;
                    end
                end
                ret = true;
            case 'char'
                ret = strcmp(a,b);
            otherwise
                ret = a == b;
            end
        end

        function ret = sequential_Q(obj)
            ret = strcmp(class(obj), 'types.List') || ...
                  strcmp(class(obj), 'types.Vector');
        end

        function ret = list_Q(obj)
            ret = strcmp(class(obj), 'types.List');
        end
        function ret = vector_Q(obj)
            ret = strcmp(class(obj), 'types.Vector');
        end
        function ret = hash_map_Q(obj)
            ret = strcmp(class(obj), 'types.HashMap');
        end

        function ret = keyword(str)
            if type_utils.keyword_Q(str)
                ret = str;
            else
                ret = sprintf('%c%s', 255, str);
            end
        end
        function ret = keyword_Q(obj)
            ret = length(obj) > 1 && strcmp(obj(1), sprintf('%c', 255));
        end

        function ret = string_Q(obj)
            ret = strcmp(class(obj), 'char') && ~type_utils.keyword_Q(obj);
        end

        function print_stack(err)
            for i=1:numel(err.stack)
                stack = err.stack(i);
                if exist('OCTAVE_VERSION', 'builtin') ~= 0
                    fprintf('    %s at line %d column %d (%s)\n', ...
                            stack.name, stack.line, stack.column, stack.file);
                else
                    fprintf('    %s at line %d (%s)\n', ...
                            stack.name, stack.line, stack.file);
                end
            end
        end
    end
end

