{zip, map, apply, and-list, join, Obj} = require 'prelude-ls'
{pr_str} = require './printer'


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
