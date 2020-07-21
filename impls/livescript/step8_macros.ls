readline = require './node_readline'
{id, map, each, last, all, unique, zip, Obj, elem-index} = require 'prelude-ls'
{read_str} = require './reader'
{pr_str} = require './printer'
{Env} = require './env'
{runtime-error, ns, unpack-tco} = require './core'
{list-to-pairs} = require './utils'


defer-tco = (env, ast) ->
    type: \tco
    env: env
    ast: ast
    eval: -> eval_ast env, ast


is-thruthy = ({type, value}) -> 
    type != \const or value not in [\nil \false]


fmap-ast = (fn, {type, value}: ast) -->
    {type: type, value: fn value}


make-symbol = (name) -> {type: \symbol, value: name}
make-list = (value) -> {type: \list, value: value}
make-call = (name, params) -> make-list [make-symbol name] ++ params
is-symbol = (ast, name) -> ast.type == \symbol and ast.value == name


eval_simple = (env, {type, value}: ast) ->
    switch type
    | \symbol => env.get value
    | \list, \vector => ast |> fmap-ast map eval_ast env
    | \map => ast |> fmap-ast Obj.map eval_ast env
    | otherwise => ast


eval_ast = (env, ast) -->
    loop
        if ast.type != \list
            return eval_simple env, ast

        ast = macroexpand env, ast
        if ast.type != \list
            return eval_simple env, ast
        else if ast.value.length == 0
            return ast

        result = if ast.value[0].type == \symbol
            params = ast.value[1 to]
            switch ast.value[0].value
            | 'def!'        => eval_def env, params
            | 'let*'        => eval_let env, params
            | 'do'          => eval_do env, params
            | 'if'          => eval_if env, params
            | 'fn*'         => eval_fn env, params
            | 'quote'       => eval_quote env, params
            | 'quasiquoteexpand' => eval_quasiquoteexpand params
            | 'quasiquote'  => eval_quasiquote env, params
            | 'defmacro!'   => eval_defmacro env, params
            | 'macroexpand' => eval_macroexpand env, params
            | otherwise     => eval_apply env, ast.value
        else 
            eval_apply env, ast.value

        if result.type == \tco
            {env, ast} = result
        else
            return result


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

    # Defer evaluation of let* body with TCO.
    defer-tco let_env, params[1]


eval_do = (env, params) ->
    if params.length == 0
        runtime-error "'do' expected at least one parameter"

    [...rest, last-param] = params
    rest |> each eval_ast env
    defer-tco env, last-param


eval_if = (env, params) ->
    if params.length < 2
        runtime-error "'if' expected at least 2 parameters"
    else if params.length > 3
        runtime-error "'if' expected at most 3 parameters"

    cond = eval_ast env, params[0]
    if is-thruthy cond
        defer-tco env, params[1]
    else if params.length > 2
        defer-tco env, params[2]
    else
        {type: \const, value: \nil}


eval_fn = (env, params) ->
    check_params 'fn*', params, 2

    if params[0].type not in [\list \vector]
        runtime-error "'fn*' expected first parameter to be a list or vector."

    if not all (.type == \symbol), params[0].value
        runtime-error "'fn*' expected only symbols in the parameters list."

    binds = params[0].value |> map (.value)
    vargs = null

    # Parse variadic bind.
    if binds.length >= 2
        [...rest, amper, name] = binds
        if amper == '&' and name != '&'
            binds = rest
            vargs = name

    if elem-index '&', binds
        runtime-error "'fn*' invalid usage of variadic parameters."

    if (unique binds).length != binds.length
        runtime-error "'fn*' duplicate symbols in parameters list."

    body = params[1]

    fn_instance = (...values) ->
        if not vargs and values.length != binds.length
            runtime-error "function expected #{binds.length} parameters, 
                           got #{values.length}"
        else if vargs and values.length < binds.length
            runtime-error "function expected at least 
                           #{binds.length} parameters, 
                           got #{values.length}"

        # Set binds to values in the new env.
        fn_env = new Env env

        for [name, value] in (zip binds, values)
            fn_env.set name, value

        if vargs
            fn_env.set vargs, 
                make-list values.slice binds.length

        # Defer evaluation of the function body to TCO.
        defer-tco fn_env, body

    {type: \function, value: fn_instance, is_macro: false}


eval_apply = (env, list) ->
    [fn, ...args] = list |> map eval_ast env
    if fn.type != \function
        runtime-error "#{fn.value} is not a function, got a #{fn.type}"

    fn.value.apply env, args


eval_quote = (env, params) ->
    if params.length != 1
        runtime-error "quote expected 1 parameter, got #{params.length}"

    params[0]


eval_quasiquoteexpand = (params) ->
    if params.length != 1
        runtime-error "quasiquote expected 1 parameter, got #{params.length}"

    ast = params[0]
    quasiquote ast


quasiquote = (ast) ->
    if ast.type in [\symbol, \map]
        make-call 'quote', [ast]
    else if ast.type == \vector
        make-call 'vec', [qq_foldr ast.value]
    else if ast.type != \list
        ast
    else if (ast.value.length == 2) and is-symbol ast.value[0], 'unquote'
        ast.value[1]
    else
        qq_foldr ast.value


qq_foldr = (xs) ->
    result = make-list []
    for i from xs.length - 1 to 0 by -1
       result := qq_loop xs[i], result
    result


qq_loop = (elt, acc) ->
    if elt.type == \list and \
       elt.value.length == 2 and \
       is-symbol elt.value[0], 'splice-unquote'
        make-call 'concat', [
            elt.value[1]
            acc
        ]
    else
        make-call 'cons', [
            quasiquote elt
            acc
        ]


eval_quasiquote = (env, params) ->
    new-ast = eval_quasiquoteexpand params
    defer-tco env, new-ast


eval_defmacro = (env, params) ->
    check_params 'def!', params, 2

    # Name is in the first parameter, and is not evaluated.
    name = params[0]
    if name.type != \symbol
        runtime-error "expected a symbol for the first parameter 
                       of defmacro!, got a #{name.type}"

    # Evaluate the second parameter.
    fn = eval_ast env, params[1]
    if fn.type != \function
        runtime-error "expected a function for the second parameter
                       of defmacro!, got a #{fn.type}"

    # Copy fn and mark the function as a macro.
    macro_fn = fn with is_macro: true
    env.set name.value, macro_fn


get-macro-fn = (env, ast) ->
    if ast.type == \list and
            ast.value.length != 0 and
            ast.value[0].type == \symbol
        fn = env.try-get ast.value[0].value
        if fn and fn.type == \function and fn.is_macro
        then fn


macroexpand = (env, ast) ->
    loop # until ast is not a macro function call.
        macro_fn = get-macro-fn env, ast
        if not macro_fn then return ast
        ast = unpack-tco <| macro_fn.value.apply env, ast.value[1 to]


eval_macroexpand = (env, params) ->
    if params.length != 1
        runtime-error "'macroexpand' expected 1 parameter, 
                       got #{params.length}"

    macroexpand env, params[0]


repl_env = new Env
for symbol, value of ns
    repl_env.set symbol, value

# Evil eval.
repl_env.set 'eval', do
    type: \function
    value: (ast) -> eval_ast repl_env, ast  # or use current env? (@ = this).

# Read, Evaluate, Print
rep = (line) ->
    line
    |> read_str
    |> eval_ast repl_env
    |> (ast) -> pr_str ast, print_readably=true


# Define not.
rep '(def! not (fn* (x) (if x false true)))'

# Define load-file.
rep '
(def! load-file 
  (fn* (f)
    (eval
      (read-string
        (str "(do " (slurp f) "\nnil)")))))'

# Define cond.
rep '
(defmacro! cond
  (fn* (& xs)
    (if (> (count xs) 0)
      (list \'if (first xs)
        (if (> (count xs) 1)
          (nth xs 1)
          (throw "odd number of forms to cond"))
        (cons \'cond (rest (rest xs)))))))'

# Parse program arguments.
# The first two (exe and core-file) are, respectively,
# the interpreter executable (nodejs or lsc) and the
# source file being executed (stepX_*.(ls|js)).
[exe, core-file, mal-file, ...argv] = process.argv

repl_env.set '*ARGV*', do
    type: \list
    value: argv |> map (arg) ->
        type: \string
        value: arg


if mal-file
    rep "(load-file \"#{mal-file}\")"
else
    # REPL.
    loop
        line = readline.readline 'user> '
        break if not line? or line == ''
        try
            console.log rep line
        catch error
            if error.message
            then console.error error.message
            else console.error "Error:", pr_str error, print_readably=true
