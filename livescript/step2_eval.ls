readline = require './node_readline'
{id, map, Obj} = require 'prelude-ls'
{read_str} = require './reader'
{pr_str} = require './printer'

repl_env = do
    '+':
        type: \function
        value: (a, b) -> {type: \int, value: a.value + b.value}
    '-': 
        type: \function
        value: (a, b) -> {type: \int, value: a.value - b.value}
    '*':
        type: \function
        value: (a, b) -> {type: \int, value: a.value * b.value}
    '/': 
        type: \function
        value: (a, b) -> {type: \int, value: parseInt(a.value / b.value)}

eval_ast = (repl_env, {type, value}: ast) -->
    switch type
    | \symbol =>
        result = repl_env[value]
        if not result? then throw new Error 'symbol not found: ', value
        result
    | \list, \vector =>
        result = value |> map eval_ast repl_env
        if type == \list and result.length != 0
            fn = result[0]
            if fn.type != \function
                throw new Error fn.value, ' is not a function'
            fn.value.apply repl_env, result.slice 1
        else
            {type: type, value: result}
    | \map =>
        {type: \map, value: value |> Obj.map eval_ast repl_env}
    | otherwise =>
        ast


rep = (line) ->
    line
    |> read_str
    |> eval_ast repl_env
    |> pr_str

loop
    line = readline.readline 'user> '
    break if not line? or line == ''
    try
        console.log rep line
    catch {message}
        console.error message
