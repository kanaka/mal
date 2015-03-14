require! {
    LiveScript
    './repl.ls': {run-repl}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
    'prelude-ls': {id}
}

read-mal = read-str

eval-mal = id

print-mal = (ast) ->
    if ast? then pr-str ast else null

run-repl read-mal >> eval-mal >> print-mal
