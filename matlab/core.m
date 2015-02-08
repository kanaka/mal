classdef core
    methods(Static)
        function str = pr_str(varargin)
            strs = cellfun(@(s) printer.pr_str(s,true), varargin, ...
                    'UniformOutput', false);
            str = strjoin(strs, ' ');
        end
        function str = do_str(varargin)
            strs = cellfun(@(s) printer.pr_str(s,false), varargin, ...
                    'UniformOutput', false);
            str = strjoin(strs, '');
        end
        function ret = prn(varargin)
            strs = cellfun(@(s) printer.pr_str(s,true), varargin, ...
                    'UniformOutput', false);
            fprintf('%s\n', strjoin(strs, ' '));
            ret = types.nil;
        end
        function ret = println(varargin)
            strs = cellfun(@(s) printer.pr_str(s,false), varargin, ...
                    'UniformOutput', false);
            fprintf('%s\n', strjoin(strs, ' '));
            ret = types.nil;
        end

        function n = ns()
            n = containers.Map();
            n('=') =  @types.equal;

            n('pr-str') = @core.pr_str;
            n('str') = @core.do_str;
            n('prn') = @core.prn;
            n('println') = @core.println;
            n('<') =  @(a,b) a<b;
            n('<=') = @(a,b) a<=b;
            n('>') =  @(a,b) a>b;
            n('>=') = @(a,b) a>=b;
            n('+') =  @(a,b) a+b;
            n('-') =  @(a,b) a-b;
            n('*') =  @(a,b) a*b;
            n('/') =  @(a,b) floor(a/b);

            n('list') = @(varargin) varargin;
            n('list?') = @iscell;
            n('empty?') = @(a) length(a) == 0;
            n('count') = @(a) length(a);
        end
    end
end

