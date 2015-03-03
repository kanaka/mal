require! {
    LiveScript
    'prelude-ls': {map, zip, partition}
    './builtins.ls': {NIL, TRUE, FALSE}
    './types.ls': {
        quasiquote, quote, unquote, splice-unquote,
        deref, keyword, with-meta,
        MalMap, MalList, MalVec
    }
}

THE_ONE_TRUE_REGEX = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/g
INTEGER = /^-?[1-9][0-9]*$/
FLOAT = /^-?[0-9]+(\.[0-9]*)?$/
STRING = /^"(\\n|\\t|\\"|\\\\|[^\\])*"$/
KEYWORD = /^:[\S]+$/

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
        | START_LIST => read-list reader
        | START_VEC => read-vec reader
        | START_MAP => read-map reader
        | EOF => null
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
    throw new MalSyntaxError "Expected #{ start }" unless start is r.next!
    while r.peek! isnt end
        if r.peek! is EOF
            throw new MalSyntaxError "Expected #{ end }"
        value.push read-form r
    r.next! # Advance past the closing paren
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
    switch t = r.next!
        | \nil   => NIL
        | \true  => TRUE
        | \false => FALSE
        | _      => switch
            | INTEGER.test t => {type: \INT,   value: (parseInt t, 10)}
            | FLOAT.test t   => {type: \FLOAT, value: (parseFloat t)}
            | STRING.test t   => {type: \STRING, value: JSON.parse(t)}
            | KEYWORD.test t => keyword t.slice 1
            | _              => {type: \SYM,   value: t}

tokenise = (str) ->
    tokens = str.match THE_ONE_TRUE_REGEX
    [trim t for t in tokens when t and (not /^\s*;/.test t)]

function trim s then s?.replace /(^[\s,]+|\s+$)/g, ''

class MalSyntaxError extends Error

    name: 'MalSyntaxError'

    (@message) ->

class Reader

    (str) -> @tokens = (tokenise str) ++ [EOF]
    
    position: 0

    next: -> @tokens[@position++]

    peek: -> @tokens[@position]

    done: -> @position >= @tokens.length

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

