readline = require 'readline'
{id, map, pairs-to-obj} = require 'prelude-ls'
{list-to-pairs} = require './utils'

export class OnlyComment

parse-error = (msg) -> throw new Error msg

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


accept-comment = (reader) ->
    token = reader.peek!
    if token? and token.startsWith ';'
        throw new OnlyComment


eof-or-comment = (reader) ->
    token = reader.peek!
    if token? and not token.startsWith ';'
    then parse-error "expected EOF, got '#{token}'"


export read_str = (str) ->
    str
    |> tokenizer
    |> (tokens) -> new Reader tokens
    |> (reader) ->
        accept-comment reader
        result = read_form reader
        eof-or-comment reader
        result


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
            parse-error "parse error at character #{idx}"

        tokens.push m[1]
    
    tokens

read_form = (reader) ->
    switch reader.peek!
    | '('  => read_list reader, ')'
    | '['  => read_list reader, ']'
    | '{'  => read_list reader, '}'
    | '\'' => read-macro 'quote', reader
    | '\`' => read-macro 'quasiquote', reader
    | '~'  => read-macro 'unquote', reader
    | '~@' => read-macro 'splice-unquote', reader
    | '@'  => read-macro 'deref', reader # todo only symbol?
    | '^'  => read-with-meta reader
    | otherwise =>
        if that? then read_atom reader
        else parse-error 'expected a form, got EOF'


read_list = (reader, end) ->
    list = []
    reader.next! # accept '(', '[' or '{'
    loop
        token = reader.peek!
        if not token?
            parse-error "expected '#{end}', got EOF"
        else if token == end
            reader.next!
            break
        
        list.push read_form reader

    switch end
    | ')' => {type: \list, value: list}
    | ']' => {type: \vector, value: list}
    | '}' => list-to-map list


special_chars = '[]{}\'`~^@'
constants = [\true \false \nil]


read_atom = (reader) ->
    token = reader.peek!
    if token in constants
        {type: \const, value: reader.next!}
    else if token[0] == '"'
        {type: \string, value: decode-string reader.next!}
    else if token.match /^-?\d+$/
        {type: \int, value: parseInt reader.next!}
    else if token != '~@' and token not in special_chars
        if token.startsWith ':'
            {type: \keyword, value: reader.next!}
        else
            {type: \symbol, value: reader.next!}
    else
        parse-error "expected an atom, got #{token}"


decode-string = (str) ->
    str |> (.slice 1, -1)
        |> (.replace /\\[\"\\n]/g,
            (esc) -> switch esc
                | '\\n' => '\n'
                | '\\"' => '"'
                | '\\\\' => '\\')


export keyword-prefix = '\u029e'


list-to-map = (list) ->
    if list.length % 2 != 0
        parse-error "map should have an even number 
                     of elements, got #{list.length}"

    list-to-pairs list
    |> map ([key, value]) ->
        switch key.type
        | \string => [key.value, value]
        | \keyword => [keyword-prefix + key.value, value]
        | otherwise =>
            parse-error "map can only have strings or keywords as keys, 
                         got a #{key.type}"
    |> pairs-to-obj
    |> (obj) -> {type: \map, value: obj}


read-macro = (symbol, reader) ->
    reader.next! # accept macro start token

    do
        type: \list
        value:
            * {type: \symbol, value: symbol}
            * read_form reader


read-with-meta = (reader) ->
    reader.next! # accept ^
    if reader.peek! != '{'
        parse-error "expected a map after with-meta reader macro '^'"

    meta = read_list reader, '}'
    form = read_form reader

    do
        type: \list
        value:
            * {type: \symbol, value: 'with-meta'}
            * form
            * meta
