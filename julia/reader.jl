module reader

export read_str

type Reader
    tokens
    position::Int64
end

function next(rdr::Reader)
    if rdr.position > length(rdr.tokens)
        return nothing
    end
    rdr.position += 1
    rdr.tokens[rdr.position-1]
end

function peek(rdr::Reader)
    if rdr.position > length(rdr.tokens)
        return nothing
    end
    rdr.tokens[rdr.position]
end


function tokenize(str)
    re = r"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)"
    tokens = map((m) -> m.captures[1], eachmatch(re, str))
    filter((t) -> t != "", tokens)
end

function read_atom(rdr)
    token = next(rdr)
    if ismatch(r"^-?[0-9]+$", token)
        int(token)
    elseif ismatch(r"^-?[0-9][0-9.]*$", token)
        float(token)
    elseif ismatch(r"^\".*\"$", token)
        replace(
            replace(token[2:end-1],
                    "\\\"", "\""),
            "\\n", "\n")
    elseif token == "nil"
        nothing
    elseif token == "true"
        true
    elseif token == "false"
        false
    else
        symbol(token)
    end
end

function read_list(rdr, start="(", last=")")
    ast = Any[]
    token = next(rdr)
    if (token != start)
        error("expected '$(start)'")
    end
    while ((token = peek(rdr)) != last)
        if token == nothing
            error("expected '$(last)', got EOF")
        end
        push!(ast, read_form(rdr))
    end
    next(rdr)
    ast
end

function read_vector(rdr)
    lst = read_list(rdr, "[", "]")
    tuple(lst...)
end

function read_hash_map(rdr)
    lst = read_list(rdr, "{", "}")
    [x[1] => x[2] for x=reshape(lst, (2, div(length(lst), 2)))]
end

function read_form(rdr)
    token = peek(rdr)
    if token == "'"
        next(rdr)
        [[:quote], Any[read_form(rdr)]]
    elseif token == "`"
        next(rdr)
        [[:quasiquote], Any[read_form(rdr)]]
    elseif token == "~"
        next(rdr)
        [[:unquote], Any[read_form(rdr)]]
    elseif token == "~@"
        next(rdr)
        [[symbol("splice-unquote")], Any[read_form(rdr)]]

    elseif token == ")"
        error("unexpected ')'")
    elseif token == "("
        read_list(rdr)
    elseif token == "]"
        error("unexpected ']'")
    elseif token == "["
        read_vector(rdr)
    elseif token == "}"
        error("unexpected '}'")
    elseif token == "{"
        read_hash_map(rdr)
    else
        read_atom(rdr)
    end
end

function read_str(str)
    tokens = tokenize(str)
    if length(tokens) == 0
        return nothing
    end
    read_form(Reader(tokens, 1))
end

end
