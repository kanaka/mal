require! {
    LiveScript
    fs
    readline
    './repl.ls': {run-repl}
    './core.ls': core
    './env.ls': {create-env}
    './types.ls': {Builtin}
    './logic.ls': {eval-mal}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
    'prelude-ls': {id}
}

{stdin, stdout} = process

read-mal = read-str

print-mal = (ast) ->
    if ast? then pr-str ast else null

rep = (env, expr) --> expr |> read-mal |> (eval-mal env) |> print-mal

core-mal = fs.readFileSync __dirname + '/core.mal', 'utf8'

let environment = create-env core.ns
    eval-mal environment, (read-str core-mal)
    run-repl rep environment
