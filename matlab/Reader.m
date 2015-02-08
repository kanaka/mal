classdef Reader < handle
    properties
        tokens
        position
    end
    methods
        function rdr = Reader(tokens)
            rdr.tokens = tokens;
            rdr.position = 1;
        end
        function tok = next(rdr)
            rdr.position = rdr.position + 1;
            if rdr.position-1 > length(rdr.tokens)
                tok = false;
            else
                tok = rdr.tokens{rdr.position-1};
            end
        end
        function tok = peek(rdr)
            if rdr.position > length(rdr.tokens)
                tok = false;
            else
                tok = rdr.tokens{rdr.position};
            end
        end
    end
end
