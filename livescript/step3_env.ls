readline = require './node_readline'
{id, map, each} = require 'prelude-ls'
{read_str} = require './reader'
{pr_str} = require './printer'
{Env} = require './env'

repl_env = new Env null, do
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


is-symbol = ({type, value}: ast, name) -> 
    type == \symbol and value == name


list-to-pairs = (list) ->
    [0 to (list.length - 2) by 2] \
        |> map (idx) -> [list[idx], list[idx+1]]


eval_ast = (env, {type, value}: ast) -->
    switch type
    | \symbol => env.get value
    | \vector => do
        type: \vector
        value: value |> map eval_ast env

    | \list =>
        # Empty list, return empty list.
        if value.length == 0
            ast
        
        # Symbol definition.
        else if is-symbol value[0], 'def!'
            if value.length != 3
                throw new Error "def! expected 2 parameters, 
                                 got #{value.length - 1}"
            
            # Name is in the first parameter, and is not evaluated.
            name = value[1]
            if name.type != \symbol
                throw new Error "expected a symbol 
                                 for the first parameter of def!, 
                                 got a #{name.type}"
            
            # Evaluate the second parameter and store 
            # it under name in the env.
            env.set name.value, (eval_ast env, value[2])

        # Create a new environment.
        else if is-symbol value[0], 'let*'
            if value.length != 3
                throw new Error "let* expected 2 parameters, 
                                 got #{value.length - 1}"

            binding_list = value[1]
            if binding_list.type not in [\list \vector]
                throw new Error "expected 1st parameter of let* to 
                                 be a binding list (or vector), 
                                 got a #{binding_list.type}"
            else if binding_list.value.length % 2 != 0
                throw new Error "binding list of let* must have an even 
                                 number of parameters"

            # Make a new environment with the 
            # current environment as outer.
            let_env = new Env env

            # Evaluate all binding values in the
            # new environment.
            binding_list.value
            |> list-to-pairs
            |> each ([binding_name, binding_value]) ->
                if binding_name.type != \symbol
                    throw new Error "expected a symbol as binding name, 
                                     got a #{binding_name.type}"

                let_env.set binding_name.value, (eval_ast let_env, binding_value)

            # Evaluate the 'body' of let* with the new environment.
            eval_ast let_env, value[2]
        else
            [fn, ...args] = value |> map eval_ast env
            if fn.type != \function
                throw new Error fn.value, ' is not a function'
            fn.value.apply env, args

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
