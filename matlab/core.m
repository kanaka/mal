classdef core
    methods(Static)
        function n = ns()
            n = containers.Map();
            n('=') =  @(a,b) a==b;
            n('<') =  @(a,b) a<b;
            n('<=') = @(a,b) a<=b;
            n('>') =  @(a,b) a>b;
            n('>=') = @(a,b) a>=b;
            n('+') = @(a,b) a+b;
            n('-') = @(a,b) a-b;
            n('*') = @(a,b) a*b;
            n('/') = @(a,b) floor(a/b);

            n('list') = @(varargin) varargin;
            n('list?') = @iscell;
            n('empty?') = @(a) length(a) == 0;
            n('count') = @(a) length(a);
        end
    end
end

