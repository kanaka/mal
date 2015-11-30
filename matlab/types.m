classdef types
    properties (Constant = true)
        nil = types.Nil();
    end

    methods(Static)
        function ret = equal(a,b)
            ret = false;
            ota = class(a); otb = class(b);
            if ~(strcmp(ota,otb) || ...
               (types.sequential_Q(a) && types.sequential_Q(b)))
                return;
            end
            switch (ota)
            case {'types.List', 'types.Vector'}
                if ~(length(a) == length(b))
                    return;
                end
                for i=1:length(a)
                    if ~(types.equal(a.get(i), b.get(i)))
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
                    if ~(types.equal(a.data(k), b.data(k)))
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
            if types.keyword_Q(str)
                ret = str;
            else
                ret = sprintf('%s%s', native2unicode(hex2dec('029e'),'UTF-8'), str);
            end
        end
        function ret = keyword_Q(obj)
            ret = length(obj) > 1 && ...
                  strcmp(obj(1), native2unicode(hex2dec('029e'),'UTF-8'));
        end
    end
end

