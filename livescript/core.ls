{zip, map, apply, and-list, join} = require 'prelude-ls'
{pr_str} = require './printer'


export runtime-error = (msg) -> throw new Error msg


fn = (body) -> {type: \function, value: body}
const-nil = {type: \const, value: \nil}
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

    'empty?': fn (param) ->
        if not list-or-vector param 
            runtime-error "'empty?' expected first parameter 
                           to be of type list or vector, 
                           got a #{param.type}."

        const-bool param.value.length == 0

    'count': fn (param) ->
        if not list-or-vector param
            runtime-error "'count' expected first parameter 
                           to be of type list or vector, 
                           got a #{param.type}."

        const-int param.value.length

    'prn': fn (param) ->
        if param
            console.log pr_str param

        const-nil

    '=': fn (a, b) -> const-bool (deep-equals a, b)
    '<': fn (a, b) -> const-bool a.value < b.value
    '>': fn (a, b) -> const-bool a.value > b.value
    '<=': fn (a, b) -> const-bool a.value <= b.value
    '>=': fn (a, b) -> const-bool a.value >= b.value

    'not': fn (a) -> const-bool (a.type == \const and a.value == \false)

    'str': fn (...params) -> const-str (params |> map pr_str |> join '')