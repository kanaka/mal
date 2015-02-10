% this is just being used as a namespace
classdef printer
    methods (Static = true)
        function str = pr_str(obj, print_readably)
            switch class(obj)
            case 'types.Symbol'
                str = obj.name;
            case 'double'
                str = num2str(obj);
            case 'char'
                if types.keyword_Q(obj)
                    str = sprintf(':%s', obj(2:end));
                else
                    if print_readably
                        str = strrep(obj, '\', '\\');
                        str = strrep(str, '"', '\"');
                        str = strrep(str, char(10), '\n');
                        str = sprintf('"%s"', str);
                    else
                        str = obj;
                    end
                end
            case 'types.List'
                strs = cellfun(@(x) printer.pr_str(x, print_readably), ...
                               obj.data, 'UniformOutput', false);
                str = sprintf('(%s)', strjoin(strs, ' '));
            case 'types.Vector'
                strs = cellfun(@(x) printer.pr_str(x, print_readably), ...
                               obj.data, 'UniformOutput', false);
                str = sprintf('[%s]', strjoin(strs, ' '));
            case 'types.HashMap'
                strs = {};
                ks = obj.keys();
                for i=1:length(ks)
                    k = ks{i};
                    strs{end+1} = printer.pr_str(k, print_readably);
                    strs{end+1} = printer.pr_str(obj.get(k), print_readably);
                end
                str = sprintf('{%s}', strjoin(strs, ' '));
            case 'types.Nil'
                str = 'nil';
            case 'logical'
                if eq(obj, true)
                    str = 'true';
                else
                    str = 'false';
                end
            case 'types.Atom'
                str = sprintf('(atom %s)', printer.pr_str(obj.val,true));
            otherwise
                str = '#<unknown>';
            end
        end
    end
end
