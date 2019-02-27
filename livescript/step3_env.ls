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


eval_simple = (env, {type, value}: ast) ->
    switch type
    | \symbol => env.get value
    | \list, \vector => do
        type: type
        value: value |> map eval_ast env
    | otherwise => ast


eval_ast = (env, {type, value}: ast) -->
    if type != \list then eval_simple env, ast
    else if value.length == 0 then ast
    else if value[0].type == \symbol
        params = value[1 to]
        switch value[0].value
        | 'def!' => eval_def env, params
        | 'let*' => eval_let env, params
        | otherwise => eval_apply env, value
    else
        eval_apply env, value


check_params = (name, params, expected) ->
    if params.length != expected
        throw new Error "#{name} expected #{expected} parameters, 
                         got #{params.length}"


eval_def = (env, params) ->
    check_params 'def!', params, 2

    # Name is in the first parameter, and is not evaluated.
    name = params[0]
    if name.type != \symbol
        throw new Error "expected a symbol for the first parameter 
                         of def!, got a #{name.type}"
    
    # Evaluate the second parameter and store 
    # it under name in the env.
    env.set name.value, (eval_ast env, params[1])


eval_let = (env, params) ->
    check_params 'let*', params, 2

    binding_list = params[0]
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
    eval_ast let_env, params[1]


eval_apply = (env, list) ->
    [fn, ...args] = list |> map eval_ast env
    if fn.type != \function
        throw new Error fn.value, ' is not a function'
    fn.value.apply env, args


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
    catch error
        if error.message
        then console.error error.message
        else console.error "Error:", pr_str error, print_readably=true
