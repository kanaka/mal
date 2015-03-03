require! {
    LiveScript
    readline
    './repl.ls': {run-repl}
    './env.ls': {create-env}
    './types.ls': {Builtin}
    './logic.ls': {eval-mal}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
    'prelude-ls': {id}
}

BUILTINS = {
    '+': (new Builtin ([x, y]) -> {type: \INT, value: x.value + y.value})
    '-': (new Builtin ([x, y]) -> {type: \INT, value: x.value - y.value})
    '*': (new Builtin ([x, y]) -> {type: \INT, value: x.value * y.value})
    '/': (new Builtin ([x, y]) -> {type: \INT, value: Math.round(x.value / y.value)})
}

{stdin, stdout} = process

read-mal = read-str

print-mal = (ast) ->
    if ast? then pr-str ast else null

rep = (env, expr) --> expr |> read-mal |> (eval-mal env) |> print-mal

run-repl rep create-env BUILTINS
