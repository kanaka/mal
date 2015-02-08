% this is just being used as a namespace
classdef reader
    methods (Static = true)
        function tokens = tokenize(str)
            re = '[\s,]*(~@|[\[\]{}()''`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}(''"`,;)]*)';
            % extract the capture group (to ignore spaces and commas)
            tokens = cellfun(@(x) x(1), regexp(str, re, 'tokens'));
        end
      
        function atm = read_atom(rdr)
            token = rdr.next();
            %fprintf('in read_atom: %s\n', token);
            if not(isempty(regexp(token, '^-?[0-9]+$', 'match')))
                atm = str2double(token);
            elseif strcmp(token(1), '"')
                atm = token(2:length(token)-1);
            %else if token eq 'nil'
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
            switch token(1)
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
