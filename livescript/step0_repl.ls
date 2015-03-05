require! {
    readline
    './repl.ls': {run-repl}
    'prelude-ls': {id}
}

{stdin, stdout} = process

read-mal = id

eval-mal = id

print-mal = id

run-repl read-mal >> eval-mal >> print-mal
