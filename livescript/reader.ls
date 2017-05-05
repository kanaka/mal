readline = require 'readline'
{id} = require 'prelude-ls'

class Reader
    (tokens) ->
        @tokens = tokens
        @pos = 0

    # returns the token at the current position
    # and increments position.
    next: ->
        result = @peek!
        if result? then @pos += 1
        result

    # just returns the token at the current position.
    peek: ->
        if @pos < @tokens.length
            @tokens[@pos]


export read_str = (str) ->
    str
    |> tokenizer
    |> (tokens) -> new Reader tokens
    |> read_form

# This function will take a single string and return an array/list 
# of all the tokens (strings) in it.
tokenizer = (str) ->
    re = //
    [\s,]*                  # whitespace or commas
    ( ~@                    # special two-char ~@
    | [\[\]{}()'`~^@]       # special single char one of []{}'`~^@
    | "(?:\\.| [^\\"])*"    # double-quoted string
    | ;.*                   # any seq of chars starting ;
    | [^\s\[\]{}('"`,;)]+   # seq of non-special chars: symbols, numbers,
    )                       # "true", "false" and "nil".
    //y

    tokens = []
    while re.lastIndex < str.length
        idx = re.lastIndex
        m = re.exec str
        # console.log 'at ', idx, 'matched', m
        if not m
            # Allow whitespace or commas at the end of the input.
            break if /[\s,]+/.exec str.substring idx
            throw new Error 'parse error at character ' + idx

        # Ignore comments.
        tok = m[1]
        if tok[1] != ';' then tokens.push tok
    
    tokens

read_form = (reader) ->
    if reader.peek! == '('
        read_list reader
    else if reader.peek!?
        read_atom reader
    else
        throw new Error 'parse error: expected a form'

read_list = (reader) ->
    list = []
    reader.next! # accept '('
    loop
        token = reader.peek!
        if not token?
            throw new Error 'expected \')\', got EOF'
        else if token == ')'
            reader.next!
            break
        
        list.push read_form reader
    
    return list

special_chars = '[]{}\'`~^@'

read_atom = (reader) ->
    token = reader.peek!
    if token[0] == '"'
        {type: \string, value: reader.next!}
    else if token.match /^-?\d+$/
        {type: \int, value: reader.next!}
    else if token != '~@' and token not in special_chars
        {type: \symbol, value: reader.next!}
    else
        throw new Error 'parse error: expected an atom, got ' + token
