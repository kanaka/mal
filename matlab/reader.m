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

        function lst = read_list(rdr)
            %fprintf('in read_list\n');
            lst = {};
            token = rdr.next();
            if not(strcmp(token, '('))
                error('expected ''(''');
            end
            token = rdr.peek();
            while true
                if eq(token, false)
                    error('expected '')''');
                end
                if strcmp(token, ')'), break, end
                lst{length(lst)+1} = reader.read_form(rdr);
                token = rdr.peek();
            end
            rdr.next();
        end

        function ast = read_form(rdr)
            %fprintf('in read_form\n');
            token = rdr.peek();
            switch token
            case ''''
                rdr.next();
                ast = {types.Symbol('quote'), reader.read_form(rdr)};
            case '`'
                rdr.next();
                ast = {types.Symbol('quasiquote'), reader.read_form(rdr)};
            case '~'
                rdr.next();
                ast = {types.Symbol('unquote'), reader.read_form(rdr)};
            case '~@'
                rdr.next();
                ast = {types.Symbol('splice-unquote'), reader.read_form(rdr)};
            case ')'
                error('unexpected '')''');
            case '('
                ast = reader.read_list(rdr);
            otherwise
                ast = reader.read_atom(rdr);
            end
        end

        function ast = read_str(str)
            %fprintf('in read_str\n');
            tokens = reader.tokenize(str);
            rdr = Reader(tokens);
            ast = reader.read_form(rdr);
        end
    end
end
