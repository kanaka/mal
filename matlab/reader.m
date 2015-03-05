% this is just being used as a namespace
classdef reader
    methods (Static = true)
        function tokens = tokenize(str)
            re = '[\s,]*(~@|[\[\]{}()''`~^@]|"(?:\\.|[^\\"])*"|;[^\n]*|[^\s\[\]{}(''"`,;)]*)';
            % extract the capture group (to ignore spaces and commas)
            tokens = cellfun(@(x) x(1), regexp(str, re, 'tokens'));
            comments = cellfun(@(x) length(x) > 0 && x(1) == ';', tokens);
            tokens = tokens(~comments);
        end
      
        function atm = read_atom(rdr)
            token = rdr.next();
            %fprintf('in read_atom: %s\n', token);
            if not(isempty(regexp(token, '^-?[0-9]+$', 'match')))
                atm = str2double(token);
            elseif strcmp(token(1), '"')
                atm = token(2:length(token)-1);
                atm = strrep(atm, '\"', '"');
                atm = strrep(atm, '\n', char(10));
            elseif strcmp(token(1), ':')
                atm = types.keyword(token);
            elseif strcmp(token, 'nil')
                atm = types.nil;
            elseif strcmp(token, 'true')
                atm = true;
            elseif strcmp(token, 'false')
                atm = false;
            else
                atm = types.Symbol(token);
            end
        end

        function seq = read_seq(rdr, start, last)
            %fprintf('in read_seq\n');
            seq = {};
            token = rdr.next();
            if not(strcmp(token, start))
                error(sprintf('expected ''%s''', start));
            end
            token = rdr.peek();
            while true
                if eq(token, false)
                    error(sprintf('expected ''%s''', last));
                end
                if strcmp(token, last), break, end
                seq{end+1} = reader.read_form(rdr);
                token = rdr.peek();
            end
            rdr.next();
        end

        function lst = read_list(rdr)
            seq = reader.read_seq(rdr, '(', ')');
            lst = types.List(seq{:});
        end

        function vec = read_vector(rdr)
            seq = reader.read_seq(rdr, '[', ']');
            vec = types.Vector(seq{:});
        end

        function map = read_hash_map(rdr)
            seq = reader.read_seq(rdr, '{', '}');
            map = types.HashMap(seq{:});
        end

        function ast = read_form(rdr)
            %fprintf('in read_form\n');
            token = rdr.peek();
            switch token
            case ''''
                rdr.next();
                ast = types.List(types.Symbol('quote'), ...
                                 reader.read_form(rdr));
            case '`'
                rdr.next();
                ast = types.List(types.Symbol('quasiquote'), ...
                                 reader.read_form(rdr));
            case '~'
                rdr.next();
                ast = types.List(types.Symbol('unquote'), ...
                                 reader.read_form(rdr));
            case '~@'
                rdr.next();
                ast = types.List(types.Symbol('splice-unquote'), ...
                                 reader.read_form(rdr));
            case '^'
                rdr.next();
                meta = reader.read_form(rdr);
                ast = types.List(types.Symbol('with-meta'), ...
                                 reader.read_form(rdr), meta);
            case '@'
                rdr.next();
                ast = types.List(types.Symbol('deref'), ...
                                 reader.read_form(rdr));

            case ')'
                error('unexpected '')''');
            case '('
                ast = reader.read_list(rdr);
            case ']'
                error('unexpected '']''');
            case '['
                ast = reader.read_vector(rdr);
            case '}'
                error('unexpected ''}''');
            case '{'
                ast = reader.read_hash_map(rdr);
            otherwise
                ast = reader.read_atom(rdr);
            end
        end

        function ast = read_str(str)
            %fprintf('in read_str\n');
            tokens = reader.tokenize(str);
            rdr = types.Reader(tokens);
            ast = reader.read_form(rdr);
        end
    end
end
