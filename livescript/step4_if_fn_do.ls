readline = require './node_readline'
{id, map, each, last, all, unique, zip} = require 'prelude-ls'
{read_str} = require './reader'
{pr_str} = require './printer'
{Env} = require './env'
{runtime-error, ns} = require './core'


is-thruthy = ({type, value}) -> 
    type != \const or value not in [\nil \false]


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
        | 'do'   => eval_do env, params
        | 'if'   => eval_if env, params
        | 'fn*'  => eval_fn env, params
        | otherwise => eval_apply env, value
    else
        eval_apply env, value


check_params = (name, params, expected) ->
    if params.length != expected
        runtime-error "'#{name}' expected #{expected} parameters, 
                       got #{params.length}"


eval_def = (env, params) ->
    check_params 'def!', params, 2

    # Name is in the first parameter, and is not evaluated.
    name = params[0]
    if name.type != \symbol
        runtime-error "expected a symbol for the first parameter 
                       of def!, got a #{name.type}"

    # Evaluate the second parameter and store 
    # it under name in the env.
    env.set name.value, (eval_ast env, params[1])


eval_let = (env, params) ->
    check_params 'let*', params, 2

    binding_list = params[0]
    if binding_list.type not in [\list \vector]
        runtime-error "expected 1st parameter of 'let*' to 
                       be a binding list (or vector), 
                       got a #{binding_list.type}"
    else if binding_list.value.length % 2 != 0
        runtime-error "binding list of 'let*' must have an even 
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
            runtime-error "expected a symbol as binding name, 
                           got a #{binding_name.type}"

        let_env.set binding_name.value, (eval_ast let_env, binding_value)

    # Evaluate the 'body' of let* with the new environment.
    eval_ast let_env, params[1]


eval_do = (env, params) ->
    if params.length == 0
        runtime-error "'do' expected at least one parameter"

    params |> map eval_ast env |> last


eval_if = (env, params) ->
    if params.length < 2
        runtime-error "'if' expected at least 2 parameters"
    else if params.length > 3
        runtime-error "'if' expected at most 3 parameters"

    cond = eval_ast env, params[0]
    if is-thruthy cond
        eval_ast env, params[1]
    else if params.length > 2
        eval_ast env, params[2]
    else
        {type: \const, value: \nil}


eval_fn = (env, params) ->
    check_params 'fn*', params, 2

    if params[0].type not in [\list \vector]
        runtime-error "'fn*' expected first parameter to be a list or vector."

    # TODO also support (& args)
    #      and (a & args)

    if not all (.type == \symbol), params[0].value
        runtime-error "'fn*' expected only symbols in the parameters list."

    binds = params[0].value |> map (.value)

    if (unique binds).length != binds.length
        runtime-error "'fn*' duplicate symbols in parameters list."

    body = params[1]

    fn_instance = (...values) ->
        if values.length != binds.length
            runtime-error "function expected #{binds.length} parameters, 
                           got #{values.length}"

        # Set binds to values in the new env.
        fn_env = new Env env
        for [name, value] in (zip binds, values)
            fn_env.set name, value

        # Evaluate the function body with the new environment.
        eval_ast fn_env, body

    {type: \function, value: fn_instance}


eval_apply = (env, list) ->
    [fn, ...args] = list |> map eval_ast env
    if fn.type != \function
        runtime-error "#{fn.value} is not a function"

    fn.value.apply env, args


repl_env = new Env
for symbol, value of ns
    repl_env.set symbol, value


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
