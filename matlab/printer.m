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
                str = strcat('"', obj, '"');
            case 'cell'
                strs = cellfun(@(x) printer.pr_str(x, print_readably), ...
                               obj, 'UniformOutput', false);
                str = strcat('(', strjoin(strs, ' '), ')');
            case 'logical'
                if eq(obj, true)
                    str = 'true';
                else
                    str = 'false';
                end
            otherwise
                str = '#<unknown>';
            end
        end
    end
end
