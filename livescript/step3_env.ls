require! {
    LiveScript
    './repl.ls': {run-repl}
    './builtins.ls': {is-seq, is-callable}
    './types.ls': {int, Builtin, MalList, MalVec, MalMap}
    './env.ls': {get-value, create-env}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
}

ENV = create-env {
    '+': (new Builtin ([x, y]) -> int x.value + y.value)
    '-': (new Builtin ([x, y]) -> int x.value - y.value)
    '*': (new Builtin ([x, y]) -> int x.value * y.value)
    '/': (new Builtin ([x, y]) -> int Math.round x.value / y.value)
}

{stdin, stdout} = process

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
    throw new Error "Cannot call #{ pr-str fn }" unless is-callable fn
    fn.fn args

do-define = (env, [name, value]) ->
    env[name.value] = EVAL env, value

do-let = (outer, [bindings, ...bodies]) ->
    inner = create-env outer
    unless is-seq bindings
        throw new Error "Bindings must be a sequence, got: #{ pr-str bindings }"

    # Set values on the inner environment.
    for i in [0 til bindings.value.length - 1 by 2]
        do-define inner, [bindings.value[i], bindings.value[i + 1]]

    # Evaluate all the bodies (implicit do)
    for body in bodies
        ret = EVAL inner, body

    # Return the last value.
    return ret

rep = (env, expr) --> expr |> read-mal |> (EVAL env) |> print-mal

run-repl (rep ENV), ENV

