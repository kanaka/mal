{zip, map, apply, and-list, join, Obj} = require 'prelude-ls'
{pr_str} = require './printer'
{read_str} = require './reader'
fs = require 'fs'


export runtime-error = (msg) -> throw new Error msg


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


check-type = (name, required-type, given-type) ->
    if required-type != given-type
        runtime-error "'#{name}' is not supported on #{given-type}"


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
        check-type 'read-string', \string, type
        read_str value

    'slurp': fn (filename) ->
        if filename.type != \string
            runtime-error "'slurp' expected the first parameter
                           to be a string, got a #{filename.type}"

        const-str <| fs.readFileSync filename.value, 'utf8'

    'atom': fn (value) -> {type: \atom, value: value}
    'atom?': fn (atom) -> const-bool atom.type == \atom
    'deref': fn (atom) ->
        check-type 'deref', \atom, atom.type
        atom.value

    'reset!': fn (atom, value) ->
        check-type 'reset!', \atom, atom.type
        atom.value = value

    'swap!': fn (atom, fn, ...args) ->
        check-type 'swap!', \atom, atom.type
        if fn.type != \function
            runtime-error "'swap!' expected the second parameter 
                           to be a function, got a #{fn.type}"

        atom.value = fn.value.apply @, [atom.value] ++ args
        if atom.value.type == \tco # TODO make this a method.
            atom.value = atom.value.eval!

        atom.value
