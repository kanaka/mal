require! {
    LiveScript
    fs
    './repl.ls': {run-repl}
    './builtins.ls': {NIL, DO, truthy, is-seq, is-callable, mal-eql}
    './core.ls': core
    './env.ls': {bind-value, get-value, create-env}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
    './types.ls': {
        int, sym, string, atom,
        Builtin, Lambda, Macro,
        MalList, MalVec, MalMap
    }
}

## The core logic of MAL

EVAL = (env, ast) --> while true
    return null unless ast? # pass nulls through (empty repl lines).
    return (eval-expr env, ast) unless ast.type is \LIST

    ast = expand-macro env, ast
    return ast if ast.type isnt \LIST

    [form, ...args] = ast.value
    throw new Error "Empty call" unless form?

    switch form.value
        | 'def!', 'def' => return do-define env, args
        | 'fn*',  'fn'  => return do-fn env, args
        | 'quote'       => return args[0]
        | 'defmacro!'   => return do-defmacro env, args
        | 'macroexpand' => return expand-macro env, args[0]
        | 'meta'        => return get-meta env, args
        | 'with-meta'   => return do-with-meta env, args
        | 'atom'        => return create-atom env, args
        | 'let*', 'let' => [env, ast] = do-let env, args
        | 'do'          => [env, ast] = do-do env, args
        | 'if'          => [env, ast] = do-if env, args
        | 'unquote'     => ast = args[0]
        | 'quasiquote'  => ast = do-quasi-quote args[0]
        | _             =>
            ret = do-call env, ast
            if ret.type is \THUNK # TCO - stay in loop.
                {env, ast} = ret
            else
                return ret

eval-expr = (env, expr) --> switch expr.type
    | \SYM => get-sym env, expr
    | \LIST => new MalList expr.value.map EVAL env
    | \VEC => new MalVec expr.value.map EVAL env
    | \MAP => new MalMap [[(EVAL env, k), (EVAL env, expr.get(k))] for k in expr.keys()]
    | _ => expr

## Atoms

create-atom = (env, [arg]) -> atom EVAL env, arg

## Meta-data

get-meta = (env, [arg]) ->
    thing = EVAL env, arg
    (thing.meta or NIL)

do-with-meta = (env, [form, metadata]) ->
    val = EVAL env, form
    data = EVAL env, (metadata or NIL)
    val.meta = data
    return val

## Read symbols from the environment, complain if they aren't there.

get-sym = (env, k) ->
    (get-value env, k) or (throw new Error "Undefined symbol: #{ pr-str k }")

## quasiquoting.

UNQUOTE = sym 'unquote'
SPLICE_UQ = sym 'splice-unquote'

do-quasi-quote = (ast) ->
    return (make-call \quote, ast) unless is-pair ast
    [head, ...tail] = ast.value
    throw new Error("Empty call") unless head
    switch
        | mal-eql UNQUOTE, head => tail[0]
        | (is-pair head) and mal-eql SPLICE_UQ, head.value[0] =>
            make-call \concat, head.value[1], (do-quasi-quote new MalList tail)
        | _ => make-call \cons, (do-quasi-quote head), (do-quasi-quote new MalList tail)

is-pair = (form) -> (is-seq form) and form.value.length

make-call = (name, ...args) -> new MalList [(sym name)] ++ args

## Function application

do-call = (env, ast) ->
    [fn, ...args] = (.value) eval-expr env, ast
    try
        switch fn.type
            | \BUILTIN => fn.fn args # Cannot thunk.
            | \LAMBDA => apply-fn fn, args
            | _ => throw new Error "Cannot call #{ pr-str fn }"
    catch e
        name = ast.value[0]
        fn-name = if name.type is \SYM
            name.value
        else
            "anonymous function"
        throw new Error "Error calling #{ fn-name }: #{ e.message }"

apply-fn = (fn, args) -> thunk (fn.closure args), (wrap-do fn.body)

thunk = (env, ast) -> {env, ast, type: \THUNK}

wrap-do = (exprs) -> new MalList [DO] ++ exprs

## Binding names to the environment.

do-define = (env, [name, value]:args) ->
    unless args.length is 2
        throw new Error "Expected 2 arguments to def, got #{ args.length } in (def! #{ args.map(pr-str).join(' ') })"
    bind-value env, name, EVAL env, value

## Macro machinery

# Form that takes a function definition, eg: (defmacro! name (fn* [] ))
do-defmacro = (env, [key, value]:args) ->
    unless args.length is 2
        throw new Error "Expected 2 arguments to defmacro!, got #{ args.length } in (defmacro! #{ args.map(pr-str).join(' ') })"
    unless key.type is \SYM
        throw new Error "name must be a symbol, got: #{ key.type }"
    fn = EVAL env, value
    unless fn instanceof Lambda
        throw new Error("Value must be a function: got #{ pr-str fn } [#{ fn.type }]")
    bind-value env, key, Macro.fromLambda fn

expand-macro = (env, ast) ->
    while is-macro-call env, ast
        [name, ...args] = ast.value
        {env, ast: body} = apply-fn (get-value env, name), args
        ast = EVAL env, body
    ast

is-macro = (env, symbol) -> (get-value env, symbol)?.type is \MACRO

is-macro-call = (env, ast) ->
    (ast?.type is \LIST) and (is-macro env, ast.value[0])

## Let form - sequential bindings

do-let = (outer, [bindings, ...bodies]) ->
    inner = create-env outer
    unless is-seq bindings
        throw new Error "Bindings must be a sequence, got: #{ pr-str bindings }"

    # Set values on the inner environment.
    for i in [0 til bindings.value.length - 1 by 2]
        do-define inner, [bindings.value[i], bindings.value[i + 1]]

    # TCO - set env to inner, wrap bodies in do.
    [inner, (wrap-do bodies)]

## multiple forms wrapped in do.

do-do = (env, [...bodies, last]) ->

    for body in bodies
        EVAL env, body

    return [env, last]

## Function definition

do-fn = (env, [names, ...body]) ->
    unless is-seq names
        throw new Error "Names must be a sequence, got: #{ pr-str names }"
    new Lambda env, names.value, body

# Basic conditional form.

do-if = (env, [test, when-true, when-false]:args) ->
    unless 2 <= args.length <= 3
        throw new Error("Wrong number of arguments to if. Expected 2-3, got #{ args.length }")
    when-false ?= NIL
    passed-test = truthy EVAL env, test
    [env, (if passed-test then when-true else when-false)]

## MAIN FUNCTIONS

read-mal = read-str

print-mal = (ast) -> if ast? then pr-str ast else null

rep = (env, expr) --> expr |> read-mal |> (EVAL env) |> print-mal

core-mal = fs.readFileSync __dirname + '/core.9.mal', 'utf8'

let env = create-env core.ns
    evaluate = EVAL env
    env['eval'] = new Builtin evaluate . (.0)
    evaluate (read-str core-mal)
    [filename, ...args] = process.argv.slice(2)
    env['*ARGV*'] = new MalList args.map(string)
    if filename
        evaluate new MalList [(sym "load-file"), (string filename)]
    else
        run-repl (rep env), env

