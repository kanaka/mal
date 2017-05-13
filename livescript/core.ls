{zip, map, apply, and-list, join, Obj, concat, all} = require 'prelude-ls'
{pr_str} = require './printer'
{read_str} = require './reader'
fs = require 'fs'


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

deep-equals = (a, b) ->
    if not list-or-vector a then
        if a.type != b.type then false
        else a.value == b.value
    else if list-or-vector b then
        if a.value.length != b.value.length then false
        else
            # Compare all elements of a and b with deep-equals.
            zip a.value, b.value
            |> map (apply deep-equals)
            |> and-list  # all must be true (equals)
    else false


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

    'not': fn ({type, value}) ->
        const-bool (type == \const and value == \false)

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
