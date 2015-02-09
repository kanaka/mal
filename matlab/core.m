classdef core
    methods(Static)
        function ret = throw(obj)
            ret = types.nil;
            throw(types.MalException(obj));
        end

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

        function ret = concat(varargin)
            if nargin == 0
                ret = {};
            else
                ret = cat(2,varargin{:});
            end
        end

        function ret = first(seq)
            if length(seq) < 1
                ret = types.nil;
            else
                ret = seq{1};
            end
        end

        function ret = nth(seq, idx)
            if idx+1 > length(seq)
                throw(MException('Range:nth', ...
                                 'nth: index out of range'))
            end
            ret = seq{idx+1};
        end

        function ret = apply(varargin)
            f = varargin{1};
            if isa(f, 'types.Function')
                f = f.fn;
            end
            first_args = varargin(2:end-1);
            rest_args = varargin{end};
            args = [first_args rest_args];
            ret = f(args{:});
        end

        function ret = map(f, lst)
            if isa(f, 'types.Function')
                f = f.fn;
            end
            ret = cellfun(@(x) f(x), lst, 'UniformOutput', false);
        end

        function n = ns()
            n = containers.Map();
            n('=') =  @types.equal;
            n('throw') = @core.throw;
            n('nil?') = @(a) isa(a, 'types.Nil');
            n('true?') = @(a) isa(a, 'logical') && a == true;
            n('false?') = @(a) isa(a, 'logical') && a == false;
            n('symbol') = @(a) types.Symbol(a);
            n('symbol?') = @(a) isa(a, 'types.Symbol');

            n('pr-str') = @core.pr_str;
            n('str') = @core.do_str;
            n('prn') = @core.prn;
            n('println') = @core.println;
            n('read-string') = @reader.read_str;
            n('readline') = @(p) input(p, 's');
            n('slurp') = @fileread;

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

            n('cons') = @(a,b) [{a}, b];
            n('concat') = @core.concat;
            n('nth') = @core.nth;
            n('first') = @core.first;
            n('rest') = @(a) a(2:end);
            n('empty?') = @(a) length(a) == 0;
            n('count') = @(a) length(a);
            n('apply') = @core.apply;
            n('map') = @core.map;
        end
    end
end

