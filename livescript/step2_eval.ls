require! {
    LiveScript
    readline
    './repl.ls': {run-repl}
    './builtins.ls': {is-callable}
    './types.ls': {int, Builtin, MalList, MalVec, MalMap}
    './env.ls': {get-value}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
    'prelude-ls': {id}
}

ENV = {
    '+': (new Builtin ([x, y]) -> int x.value + y.value)
    '-': (new Builtin ([x, y]) -> int x.value - y.value)
    '*': (new Builtin ([x, y]) -> int x.value * y.value)
    '/': (new Builtin ([x, y]) -> int Math.round x.value / y.value)
}

{stdin, stdout} = process

read-mal = read-str

print-mal = (ast) -> if ast? then pr-str ast else null

eval-expr = (env, expr) --> switch expr.type
    | \SYM => (get-value env, expr) or (throw new Error "Undefined symbol: #{ pr-str expr }")
    | \LIST => new MalList expr.value.map EVAL env
    | \VEC => new MalVec expr.value.map EVAL env
    | \MAP => new MalMap [[(EVAL env, k), (EVAL env, expr.get(k))] for k in expr.keys()]
    | _ => expr

EVAL = (env, form) --> switch form.type
    | \LIST =>
        [fn, ...args] = (.value) eval-expr env, form
        throw new Error "Empty call" unless fn?
        throw new Error "Cannot call #{ pr-str fn }" unless is-callable fn
        fn.fn args
    | _ => eval-expr env, form

rep = (env, expr) --> expr |> read-mal |> (EVAL env) |> print-mal

run-repl (rep ENV), ENV

