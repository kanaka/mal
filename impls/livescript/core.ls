
{
    zip, map, apply, and-list, join, Obj, concat, all,
    pairs-to-obj, obj-to-pairs, reject, keys, values,
    difference, empty, reverse, chars
} = require 'prelude-ls'
{pr_str} = require './printer'
{read_str, list-to-map, map-keyword, keyword-prefix} = require './reader'
fs = require 'fs'
{readline} = require './node_readline'


export runtime-error = (msg) -> throw new Error msg

export unpack-tco = (ast) ->
    if ast.type == \tco
    then ast.eval!
    else ast

fn = (body) -> {type: \function, value: body}
const-nil = -> {type: \const, value: \nil}
const-int = (int) -> {type: \int, value: int}
const-bool = (bool) -> {type: \const, value: if bool then \true else \false}
const-str = (str) -> {type: \string, value: str}

list-or-vector = ({type}) -> type in [\list \vector]

are-lists-equal = (equals-fn, a, b) ->
    if a.length != b.length then false
    else zip a, b |> map (apply equals-fn) |> and-list

deep-equals = (a, b) ->
    if (list-or-vector a) and (list-or-vector b) then
        are-lists-equal deep-equals, a.value, b.value
    else if a.type == \map and b.type == \map then
        a-keys = keys a.value
        b-keys = keys b.value
        if a-keys.length == b-keys.length and \
            empty (difference a-keys, b-keys)
            #if are-lists-equal (==), a-keys, b-keys
            a-keys |> map (key) -> [a.value[key], b.value[key]]
                   |> map (apply deep-equals)
                   |> and-list
        else false
    else if a.type != b.type then false
    else a.value == b.value


check-param = (name, idx, test, expected, actual) ->
    if not test
        runtime-error "'#{name}' expected parameter #{idx} 
                       to be #{expected}, got #{actual}"


check-type = (name, idx, expected, actual) ->
    check-param name, idx, expected == actual, expected, actual


export ns = do
    '+': fn (a, b) -> const-int a.value + b.value
    '-': fn (a, b) -> const-int a.value - b.value
    '*': fn (a, b) -> const-int a.value * b.value
    '/': fn (a, b) -> const-int parseInt (a.value / b.value)

    'list': fn (...list) -> {type: \list, value: list}
    'list?': fn (param) -> const-bool param.type == \list

    'empty?': fn ({type, value}) ->
        switch type
        | \const =>
            if value == \nil
            then const-bool true
            else runtime-error "'empty?' is not supported on #{value}"
        | \list, \vector =>
            const-bool value.length == 0
        | \map =>
            const-bool Obj.empty value
        | otherwise =>
            runtime-error "'empty?' is not supported on type #{type}"

    'count': fn ({type, value}) ->
        switch type
        | \const =>
            if value == \nil
            then const-int 0
            else runtime-error "'count' is not supported on #{value}"
        | \list, \vector =>
            const-int value.length
        | \map =>
            value |> Obj.keys |> (.length) |> const-int
        | otherwise =>
            runtime-error "'count' is not supported on type #{type}"

    '=': fn (a, b) -> const-bool (deep-equals a, b)
    '<': fn (a, b) -> const-bool a.value < b.value
    '>': fn (a, b) -> const-bool a.value > b.value
    '<=': fn (a, b) -> const-bool a.value <= b.value
    '>=': fn (a, b) -> const-bool a.value >= b.value

    'pr-str': fn (...params) ->
        params |> map (p) -> pr_str p, print_readably=true
               |> join ' '
               |> const-str

    'str': fn (...params) ->
        params |> map (p) -> pr_str p, print_readably=false
               |> join ''
               |> const-str

    'prn': fn (...params) ->
        params |> map (p) -> pr_str p, print_readably=true
               |> join ' '
               |> console.log
               |> const-nil

    'println': fn (...params) ->
        params |> map (p) -> pr_str p, print_readbly=false
               |> join ' '
               |> console.log
               |> const-nil

    'read-string': fn ({type, value}) ->
        check-type 'read-string', 0, \string, type
        read_str value

    'slurp': fn (filename) ->
        if filename.type != \string
            runtime-error "'slurp' expected the first parameter
                           to be a string, got a #{filename.type}"

        const-str <| fs.readFileSync filename.value, 'utf8'

    'atom': fn (value) -> {type: \atom, value: value}
    'atom?': fn (atom) -> const-bool atom.type == \atom
    'deref': fn (atom) ->
        check-type 'deref', 0, \atom, atom.type
        atom.value

    'reset!': fn (atom, value) ->
        check-type 'reset!', 0, \atom, atom.type
        atom.value = value

    'swap!': fn (atom, fn, ...args) ->
        check-type 'swap!', 0, \atom, atom.type
        if fn.type != \function
            runtime-error "'swap!' expected the second parameter 
                           to be a function, got a #{fn.type}"

        atom.value = unpack-tco (fn.value.apply @, [atom.value] ++ args)

    'cons': fn (value, list) ->
        check-param 'cons', 1, (list-or-vector list),
            'list or vector', list.type

        {type: \list, value: [value] ++ list.value}

    'concat': fn (...params) ->
        if not all list-or-vector, params
            runtime-error "'concat' expected all parameters to be a list or vector"

        {type: \list, value: params |> map (.value) |> concat}

    'nth': fn (list, index) ->
        check-param 'nth', 0, (list-or-vector list),
            'list or vector', list.type
        check-param 'nth', 1, index.type == \int,
            'int', index.type

        if index.value < 0 or index.value >= list.value.length
            runtime-error 'list index out of bounds'

        list.value[index.value]

    'first': fn (list) ->
        if list.type == \const and list.value == \nil
            return const-nil!

        check-param 'first', 0, (list-or-vector list),
            'list or vector', list.type

        if list.value.length == 0
        then const-nil!
        else list.value[0]

    'rest': fn (list) ->
        if list.type == \const and list.value == \nil
            return {type: \list, value: []}

        check-param 'rest', 0, (list-or-vector list),
            'list or vector', list.type

        {type: \list, value: list.value.slice 1}

    'throw': fn (value) -> throw value

    'apply': fn (fn, ...params, list) ->
        check-type 'apply', 0, \function, fn.type
        if not list then runtime-error "apply expected at least two parameters"
        check-param 'apply', params.length+1, (list-or-vector list),
            'list or vector', list.type

        unpack-tco fn.value.apply @, params ++ list.value

    'map': fn (fn, list) ->
        check-type 'map', 0, \function, fn.type
        check-param 'map', 1, (list-or-vector list),
            'list or vector', list.type

        mapped-list = list.value |> map (value) ->
            unpack-tco fn.value.apply @, [value]

        {type: \list, value: mapped-list}

    'nil?': fn (ast) -> const-bool (ast.type == \const and ast.value == \nil)
    'true?': fn (ast) -> const-bool (ast.type == \const and ast.value == \true)
    'false?': fn (ast) -> const-bool (ast.type == \const and ast.value == \false)
    'symbol?': fn (ast) -> const-bool ast.type == \symbol

    'symbol': fn (str) ->
        check-type 'symbol', 0, \string, str.type
        {type: \symbol, value: str.value}

    'keyword': fn (str) ->
        check-type 'keyword', 0, \string, str.type
        {type: \keyword, value: ':' + str.value}

    'keyword?': fn (ast) -> const-bool ast.type == \keyword

    'number?': fn (ast) -> const-bool ast.type == \int
    'fn?': fn (ast) -> const-bool (ast.type == \function and not ast.is_macro)
    'macro?': fn (ast) -> const-bool (ast.type == \function and ast.is_macro)

    'vector': fn (...params) -> {type: \vector, value: params}
    'vector?': fn (ast) -> const-bool ast.type == \vector

    'hash-map': fn (...params) -> list-to-map params

    'map?': fn (ast) -> const-bool ast.type == \map

    'assoc': fn (m, ...params) ->
        check-type 'assoc', 0, \map, m.type

        # Turn the params into a map, this is kind of hacky.
        params-map = list-to-map params

        # Copy the map by cloning (prototyping).
        new-map = ^^m.value

        for k, v of params-map.value
            new-map[k] = v

        {type: \map, value: new-map}

    'dissoc': fn (m, ...keys) ->
        check-type 'dissoc', 0, \map, m.type

        # Convert keyword to map key strings.
        str-keys = keys |> map map-keyword

        new-map = m.value
            |> obj-to-pairs
            |> reject ([key, value]) -> key in str-keys
            |> pairs-to-obj

        {type: \map, value: new-map}

    'get': fn (m, key) ->
        if m.type == \const and m.value == \nil
        then return const-nil!

        check-type 'get', 0, \map, m.type
        str-key = map-keyword key
        value = m.value[str-key]
        if value then value else const-nil!

    'contains?': fn (m, key) ->
        check-type 'contains?', 0, \map, m.type
        str-key = map-keyword key
        const-bool (str-key of m.value)

    'keys': fn (m) ->
        check-type 'keys', 0, \map, m.type
        result = keys m.value |> map (key) ->
            if key.startsWith keyword-prefix
            then {type: \keyword, value: key.substring 1}
            else {type: \string, value: key}
        {type: \list, value: result}

    'vals': fn (m) ->
        check-type 'vals', 0, \map, m.type
        {type: \list, value: values m.value}

    'sequential?': fn (ast) -> const-bool list-or-vector ast

    'with-meta': fn (ast, m) ->
        ast with {meta: m}

    'meta': fn (ast) ->
        if ast.meta
        then ast.meta
        else const-nil!

    'readline': fn (prompt) ->
        check-type 'readline', 0, \string, prompt.type
        result = readline prompt.value
        if result?
        then const-str result
        else const-nil!

    'time-ms': fn ->
        const-int (new Date).getTime!

    'conj': fn (list, ...params) ->
        check-param 'conj', 0, (list-or-vector list),
            'list or vector', list.type

        if list.type == \list
            type: \list
            value: (reverse params) ++ list.value
        else
            type: \vector
            value: list.value ++ params

    'string?': fn (ast) -> const-bool ast.type == \string

    'seq': fn (seq) ->
        switch seq.type
        | \list =>
            if seq.value.length
            then seq
            else const-nil!
        | \vector =>
            if seq.value.length
            then {type: \list, value: seq.value}
            else const-nil!
        | \string =>
            if seq.value.length
            then {type: \list, value: chars seq.value |> map const-str}
            else const-nil!
        | otherwise =>
            if seq.type == \const and seq.value == \nil
            then const-nil!
            else runtime-error "unsupported type for 'seq': #{seq.type}"
