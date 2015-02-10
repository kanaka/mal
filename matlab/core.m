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
            remove(new_hm.data, ks);
        end

        function ret = get(hm, key)
            if hm == types.nil
                ret = types.nil;
            else
                if hm.data.isKey(key)
                    ret = hm.data(key);
                else
                    ret = types.nil;
                end
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
            if length(seq) < 1
                ret = types.nil;
            else
                ret = seq.get(1);
            end
        end

        function ret = rest(seq)
            cella = seq.data(2:end);
            ret = types.List(cella{:});
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
                meta = types.nil;
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
            n = containers.Map();
            n('=') =  @types.equal;
            n('throw') = @core.throw;
            n('nil?') = @(a) isa(a, 'types.Nil');
            n('true?') = @(a) isa(a, 'logical') && a == true;
            n('false?') = @(a) isa(a, 'logical') && a == false;
            n('symbol') = @(a) types.Symbol(a);
            n('symbol?') = @(a) isa(a, 'types.Symbol');
            n('keyword') = @types.keyword;
            n('keyword?') = @types.keyword_Q;

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
            n('time-ms') = @core.time_ms;

            n('list') = @(varargin) types.List(varargin{:});
            n('list?') = @types.list_Q;
            n('vector') = @(varargin) types.Vector(varargin{:});
            n('vector?') = @types.vector_Q;
            n('hash-map') = @(varargin) types.HashMap(varargin{:});
            n('map?') = @types.hash_map_Q;
            n('assoc') = @core.assoc;
            n('dissoc') = @core.dissoc;
            n('get') = @core.get;
            n('contains?') = @(a,b) a.data.isKey(b);
            n('keys') = @core.keys;
            n('vals') = @core.vals;

            n('sequential?') = @types.sequential_Q;
            n('cons') = @core.cons;
            n('concat') = @core.concat;
            n('nth') = @core.nth;
            n('first') = @core.first;
            n('rest') = @core.rest;
            n('empty?') = @(a) length(a) == 0;
            n('count') = @(a) length(a);
            n('apply') = @core.apply;
            n('map') = @core.map;
            n('conj') = @(x) disp('not implemented yet');

            n('with-meta') = @core.with_meta;
            n('meta') = @core.meta;
            n('atom') = @types.Atom;
            n('atom?') = @(a) isa(a, 'types.Atom');
            n('deref') = @(a) a.val;
            n('reset!') = @core.reset_BANG;
            n('swap!') = @core.swap_BANG;
        end
    end
end

