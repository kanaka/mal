classdef core
    methods(Static)
        function ret = throw(obj)
            ret = type_utils.nil;
            if exist('OCTAVE_VERSION', 'builtin') ~= 0
                % Until Octave has MException objects, we need to
                % store the error object globally to be able to pass
                % it to the error handler.
                global error_object;
                error_object = obj;
                exc = struct('identifier', 'MalException:object',...
                             'message', 'MalException');
                rethrow(exc);
            else
                throw(types.MalException(obj));
            end
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
            ret = type_utils.nil;
        end
        function ret = println(varargin)
            strs = cellfun(@(s) printer.pr_str(s,false), varargin, ...
                    'UniformOutput', false);
            fprintf('%s\n', strjoin(strs, ' '));
            ret = type_utils.nil;
        end

        function ret = time_ms()
            secs = now-repmat(datenum('1970-1-1 00:00:00'),size(now));
            ret = floor(secs.*repmat(24*3600.0*1000,size(now)));
        end

        function new_hm = assoc(hm, varargin)
            new_hm = clone(hm);
            for i=1:2:length(varargin)
                new_hm.set(varargin{i}, varargin{i+1});
            end
        end

        function new_hm = dissoc(hm, varargin)
            new_hm = clone(hm);
            ks = intersect(hm.keys(),varargin);
            if exist('OCTAVE_VERSION', 'builtin') ~= 0
                new_hm.data.remove(ks);
            else
                remove(new_hm.data, ks);
            end
        end

        function ret = get(hm, key)
            if isa(hm, 'types.Nil')
                ret = type_utils.nil;
            elseif hm.data.isKey(key)
                ret = hm.data(key);
            else
                ret = type_utils.nil;
            end
        end

        function ret = keys(hm)
            ks = hm.keys();
            ret = types.List(ks{:});
        end

        function ret = vals(hm)
            vs = hm.values();
            ret = types.List(vs{:});
        end

        function ret = cons(a, seq)
            cella = [{a}, seq.data];
            ret = types.List(cella{:});
        end

        function ret = concat(varargin)
            if nargin == 0
                cella = {};
            else
                cells = cellfun(@(x) x.data, varargin, ...
                                'UniformOutput', false);
                cella = cat(2,cells{:});
            end
            ret = types.List(cella{:});
        end

        function ret = first(seq)
            if isa(seq, 'types.Nil')
                ret = type_utils.nil;
            elseif length(seq) < 1
                ret = type_utils.nil;
            else
                ret = seq.get(1);
            end
        end

        function ret = rest(seq)
            if isa(seq, 'types.Nil')
                ret = types.List();
            else
                cella = seq.data(2:end);
                ret = types.List(cella{:});
            end
        end

        function ret = nth(seq, idx)
            if idx+1 > length(seq)
                throw(MException('Range:nth', ...
                                 'nth: index out of range'))
            end
            ret = seq.get(idx+1);
        end

        function ret = apply(varargin)
            f = varargin{1};
            if isa(f, 'types.Function')
                f = f.fn;
            end
            first_args = varargin(2:end-1);
            rest_args = varargin{end}.data;
            args = [first_args rest_args];
            ret = f(args{:});
        end

        function ret = map(f, lst)
            if isa(f, 'types.Function')
                f = f.fn;
            end
            cells = cellfun(@(x) f(x), lst.data, 'UniformOutput', false);
            ret = types.List(cells{:});
        end

        function new_obj = with_meta(obj, meta)
            new_obj = clone(obj);
            new_obj.meta = meta;
        end

        function meta = meta(obj)
            switch class(obj)
            case {'types.List', 'types.Vector',
                  'types.HashMap', 'types.Function'}
                meta = obj.meta;
            otherwise
                meta = type_utils.nil;
            end
        end

        function ret = reset_BANG(atm, val)
            atm.val = val;
            ret = val;
        end

        function ret = swap_BANG(atm, f, varargin)
            args = [{atm.val} varargin];
            if isa(f, 'types.Function')
                f = f.fn;
            end
            atm.val = f(args{:});
            ret = atm.val;
        end

        function n = ns()
            if exist('OCTAVE_VERSION', 'builtin') ~= 0
                n = Dict();
            else
                n = containers.Map();
            end
            n('=') =  @(a,b) type_utils.equal(a,b);
            n('throw') = @(a) core.throw(a);
            n('nil?') = @(a) isa(a, 'types.Nil');
            n('true?') = @(a) isa(a, 'logical') && a == true;
            n('false?') = @(a) isa(a, 'logical') && a == false;
            n('symbol') = @(a) types.Symbol(a);
            n('symbol?') = @(a) isa(a, 'types.Symbol');
            n('keyword') = @(a) type_utils.keyword(a);
            n('keyword?') = @(a) type_utils.keyword_Q(a);

            n('pr-str') = @(varargin) core.pr_str(varargin{:});
            n('str') = @(varargin) core.do_str(varargin{:});
            n('prn') = @(varargin) core.prn(varargin{:});
            n('println') = @(varargin) core.println(varargin{:});
            n('read-string') = @(a) reader.read_str(a);
            n('readline') = @(p) input(p, 's');
            n('slurp') = @(a) fileread(a);

            n('<') =  @(a,b) a<b;
            n('<=') = @(a,b) a<=b;
            n('>') =  @(a,b) a>b;
            n('>=') = @(a,b) a>=b;
            n('+') =  @(a,b) a+b;
            n('-') =  @(a,b) a-b;
            n('*') =  @(a,b) a*b;
            n('/') =  @(a,b) floor(a/b);
            n('time-ms') = @() core.time_ms();

            n('list') = @(varargin) types.List(varargin{:});
            n('list?') = @(a) type_utils.list_Q(a);
            n('vector') = @(varargin) types.Vector(varargin{:});
            n('vector?') = @(a) type_utils.vector_Q(a);
            n('hash-map') = @(varargin) types.HashMap(varargin{:});
            n('map?') = @(a) type_utils.hash_map_Q(a);
            n('assoc') = @(varargin) core.assoc(varargin{:});
            n('dissoc') = @(varargin) core.dissoc(varargin{:});
            n('get') = @(a,b) core.get(a,b);
            n('contains?') = @(a,b) a.data.isKey(b);
            n('keys') = @(a) core.keys(a);
            n('vals') = @(a) core.vals(a);

            n('sequential?') = @(a) type_utils.sequential_Q(a);
            n('cons') = @(a,b) core.cons(a,b);
            n('concat') = @(varargin) core.concat(varargin{:});
            n('nth') = @(a,b) core.nth(a,b);
            n('first') = @(a) core.first(a);
            n('rest') = @(a) core.rest(a);
            n('empty?') = @(a) length(a) == 0;
            % workaround Octave always giving length(a) of 1
            n('count') = @(a) 0 + length(a);
            n('apply') = @(varargin) core.apply(varargin{:});
            n('map') = @(varargin) core.map(varargin{:});
            n('conj') = @(x) disp('not implemented yet');

            n('with-meta') = @(a,b) core.with_meta(a,b);
            n('meta') = @(a) core.meta(a);
            n('atom') = @(a) types.Atom(a);
            n('atom?') = @(a) isa(a, 'types.Atom');
            n('deref') = @(a) a.val;
            n('reset!') = @(a,b) core.reset_BANG(a,b);
            n('swap!') = @(varargin) core.swap_BANG(varargin{:});
        end
    end
end

