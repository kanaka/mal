require! {
    LiveScript
    './repl.ls': {run-repl}
    './builtins.ls': {NIL, DO, truthy, is-seq, is-callable}
    './types.ls': {int, Builtin, Lambda, MalList, MalVec, MalMap}
    './core.ls': core
    './env.ls': {get-value, create-env}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
}

read-mal = read-str

print-mal = (ast) -> if ast? then pr-str ast else null

EVAL = (env, ast) -->
    return null unless ast? # pass nulls through (empty repl lines).
    return (eval-expr env, ast) unless ast.type is \LIST
    [form, ...args] = ast.value
    throw new Error "Empty call" unless form?

    switch form.value
        | 'def!' => do-define env, args
        | 'let*' => do-let env, args
        | 'do' => do-do env, args
        | 'if' => do-if env, args
        | 'fn*' => do-fn env, args
        | _ => do-call env, ast

eval-expr = (env, expr) --> switch expr.type
    | \SYM => get-sym env, expr
    | \LIST => new MalList expr.value.map EVAL env
    | \VEC => new MalVec expr.value.map EVAL env
    | \MAP => new MalMap [[(EVAL env, k), (EVAL env, expr.get(k))] for k in expr.keys()]
    | _ => expr

get-sym = (env, key) ->
    (get-value env, key) or (throw new Error "Undefined symbol: #{ pr-str key }")

do-call = (env, ast) ->
    [fn, ...args] = (.value) eval-expr env, ast
    switch fn.type
        | \BUILTIN => fn.fn args
        | \LAMBDA => EVAL (fn.closure args), (wrap-do fn.body)
        | _ => throw new Error "Cannot call #{ pr-str fn }"

wrap-do = (exprs) -> new MalList [DO] ++ exprs

do-define = (env, [name, value]) ->
    env[name.value] = EVAL env, value

do-let = (outer, [bindings, ...bodies]) ->
    inner = create-env outer
    unless is-seq bindings
        throw new Error "Bindings must be a sequence, got: #{ pr-str bindings }"

    # Set values on the inner environment.
    for i in [0 til bindings.value.length - 1 by 2]
        do-define inner, [bindings.value[i], bindings.value[i + 1]]

    # Evaluate all the bodies (implicit do), returning the last one.
    do-do inner, bodies

do-do = (env, bodies) ->

    for body in bodies
        ret = EVAL env, body

    return ret

do-fn = (env, [names, ...body]) ->
    unless is-seq names
        throw new Error "Names must be a sequence, got: #{ pr-str names }"
    new Lambda env, names.value, body

do-if = (env, [test, when-true, when-false]:args) ->
    throw new Error("Wrong number of arguments to if. Expected 2-3, got #{ args.length }") unless 2 <= args.length <= 3
    when-false ?= NIL
    passed-test = truthy EVAL env, test
    EVAL env, (if passed-test then when-true else when-false)

rep = (env, expr) --> expr |> read-mal |> (EVAL env) |> print-mal

let env = create-env core.ns
    env.not = "(fn* [x] (if x false true))" |> read-mal |> (EVAL env)
    run-repl (rep env), env

