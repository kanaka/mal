require! {
    LiveScript
    'prelude-ls': {map, zip, partition}
    './builtins.ls': {NIL, TRUE, FALSE}
    './types.ls': {
        quasiquote, quote, unquote, splice-unquote,
        deref, with-meta,
        string, int, float, sym, keyword,
        MalMap, MalList, MalVec
    }
}

INTEGER = /^-?([1-9][0-9]*|0)$/
FLOAT = /^-?[0-9]+(\.[0-9]*)?$/

QUOTE = '\''
DEREF = '@'
QUASIQUOTE = '`'
UNQUOTE = '~'
SPLICE_UNQUOTE = '~@'
WITH_META = '^'
[START_LIST, END_LIST] = ['(', ')']
[START_VEC, END_VEC]   = ['[', ']']
[START_MAP, END_MAP]   = ['{', '}']

EOF = {} # Unique token

export read-str = (str) ->
    read-form new Reader str

read-form = (reader) ->
    if process.env.DEBUG
        console.log reader.position, reader.peek!
    switch reader.peek!
        | EOF => null
        | START_LIST => read-list reader
        | START_VEC => read-vec reader
        | START_MAP => read-map reader
        | QUOTE => read-quote reader
        | QUASIQUOTE => read-quasiquote reader
        | UNQUOTE => read-unquote reader
        | SPLICE_UNQUOTE => read-splice-unquote reader
        | WITH_META => read-with-meta reader
        | DEREF => read-deref reader
        | otherwise => read-atom reader

read-special = (token, wrap, r) -->
    throw new Error unless token is r.next!
    wrap read-form r

read-quote = read-special QUOTE, quote
read-quasiquote = read-special QUASIQUOTE, quasiquote
read-unquote = read-special UNQUOTE, unquote
read-splice-unquote = read-special SPLICE_UNQUOTE, splice-unquote

read-with-meta = (r) ->
    throw new Error unless WITH_META is r.next!
    meta = read-form r
    if r.peek! is EOF
        throw new MalSyntaxError "Expected form for meta-data, got EOF"
    data = read-form r
    with-meta meta, data

read-deref = (r) ->
    throw new Error unless DEREF is r.next!
    deref r.next!

read-seq = (Coll, start, end, r) -->
    value = []
    # Check and advance
    throw new MalSyntaxError "Expected #{ start }" unless start is r.peek!
    while r.next! isnt end
        if r.peek! is EOF
            throw new MalSyntaxError "Expected #{ end }, at #{ r.index }, got #{ JSON.stringify r.current }"
        value.push read-form r
    new Coll value

read-list = read-seq MalList, START_LIST, END_LIST

read-vec = read-seq MalVec, START_VEC, END_VEC

read-map = (r) ->
    {value:vs} = read-seq MalList, START_MAP, END_MAP, r
    if vs.length % 2
        throw new MalSyntaxError 'Maps must have even number of elements'
    new MalMap (to-pairs vs)

# [k, v, k, v...] -> [[k, v]]
to-pairs = (xs) -> zip.apply null, keys-and-vals xs
# [k, v, k, v...] -> [[k], [v]]
keys-and-vals = (xs) ->
    [0 to xs.length] |> (zip xs) |> ks-vs |> discard-indices

ks-vs = partition (.1) >> (% 2) >> (is 0)
discard-indices = map (map (.0))

read-atom = (r) ->
    t = r.peek!
    if t is EOF
        throw new Error "EOF in ATOM"
    throw new Error("Illegal atom: #{ JSON.stringify t }") unless t?.type
    return t unless t.type is \SYM # done by reader.
    switch t.value # Read out built in constants.
        | \nil   => NIL
        | \true  => TRUE
        | \false => FALSE
        | _ => t

class MalSyntaxError extends Error

    name: 'MalSyntaxError'

    (@message) ->

export class Reader

    (@str) ->

    index: 0

    incr: 1

    position: -1

    current: null

    next: ->
        @advance! unless @done!
        @current

    peek: ->
        @advance! if @position is -1
        @current

    done: -> @peek! is EOF

    advance: ->
        # Skip whitespace.
        while is-whitespace @read-current-char!
            @advance-index!

        if @index >= @str.length
            return @current = EOF

        beginning = @index
        char = @read-current-char!
        @position++
        @current = switch char
            | EOF => char
            | <[ { } ( ) [ ] ' ` ~ ~@ @ ^ ]> => @advance-index!; char
            | ':' => keyword @read-kw!
            | '"' => string @read-string!
            | _ => switch
                | is-numeric char =>
                    num = @read-number!
                    switch
                        | (not /\d/.test num) => sym num + @read-symbol!
                        | INTEGER.test num => int parseInt num, 10
                        | FLOAT.test num => float parseFloat num
                        | _ => throw new Error("Illegal numeric token at #{ beginning }: #{ num }")
                | is-symbolic char => sym @read-symbol!
                | _ => throw new Error("Unexpected token: #{ char }")

    advance-index: ->
        @index += @incr
        @incr = 1

    read-kw: ->
        char = @read-current-char!
        throw new Error('Not a keyword') unless char is ':'
        @advance-index!
        @read-symbol!

    read-string: ->
        chars = []
        ch = @read-current-char!
        throw new Error('Not a string') unless ch is '"'
        @advance-index!
        ch = @read-current-char!
        while ch isnt '"'
            if ch is '\\'
                @advance-index!
                next-ch = @read-current-char!
                if next-ch in <[ n t " ' \ ]>
                    chars.push unescape next-ch
                else
                    throw new Error("Unexpected escape: \\#{ next-ch }")
            else
                chars.push ch
            ch = @read-next-char!
        @advance-index!
        chars.join ''

    read-symbol: -> @read-all is-symbolic

    read-number: -> @read-all is-numeric

    read-all: (test) ->
        chars = []
        ch = @read-current-char!
        while test ch
            chars.push ch
            ch = @read-next-char!
        return chars.join ''

    read-next-char: ->
        @advance-index!
        ch = @read-current-char!

    read-current-char: ->
        i = @index
        [char, incr] = get-whole-char @str, i
        @incr = incr # save the increment for the next step.
        char

function unescape char then switch char
    | 'n' => '\n'
    | 't' => '\t'
    | _ => char

function is-whitespace char then /^(\s|,)*$/.test char

function is-symbolic char then (/^[\w\d]+$/.test char) or (char in <[ $ ! _ % & \ * + - / : < = > ? @ ^ _ ~ . ]>)

function is-numeric char then /^[\d\.-]+$/.test char

function get-whole-char str, i then
    throw new Error('Index out of bounds: i < 0') if i < 0
    code = str.charCodeAt i
    switch
        | isNaN code => [EOF, 1] # Not found - end of input.
        | (code < 0xD800) or (0xDFFF < code)  => [str.charAt(i), 1] # Normal character 
        | 0xD800 <= code <= 0xDBFF => [(high-surrogate str, i), 2]
        | _ => [(low-surrogate str, i), 2]

function high-surrogate str, i then
    j = i + 1
    next = str.charCodeAt j
    throw new Error('High surrogate without following low surrogate') if str.length <= j
    throw new Error('High surrogate without following low surrogate') if (0xDC00 > next) or (next > 0xDFFF)
    str.charAt(i) + str.charAt(j)

function low-surrogate str, i then
    throw new Error('Low surrogate without preceding high surrogate') if i is 0
    prev = str.charCodeAt(i - 1);
    throw new Error('Low surrogate without preceding high surrogate') if (0xD800 > prev) or (prev > 0xDBFF)
    # Return the next character instead
    str.charAt(i + 1)

unless module.parent
    examples = [
        'foo',
        '"foo bar"',
        '"foo\\tbar"',
        '12',
        '1.2',
        '(+ 1 2)',
        '(+ 1 (* 3 4))',
        '[1 2 3 (+ 2 3)]',
        '{x 1 y 2 1.2 + [a b] 3 nil {2 3}}'
    ]
    for eg in examples
        console.log eg, (read-str eg)
    console.log 'EMPTY', (read-str '')
    console.log 'KEYS', read-str('{1 2}').keys()
    try
        console.log '(a b c', (read-str '(a b c')
    catch e
        console.log e
    try
        console.log '{x}', (read-str '{x}')
    catch e
        console.log e

