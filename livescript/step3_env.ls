require! {
    LiveScript
    readline
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
    '/': (new Builtin ([x, y]) -> {type: \INT, value: x.value / y.value})
}

{stdin, stdout} = process

read-mal = read-str

print-mal = (ast) ->
    if ast? then pr-str ast else null

rep = (expr, env) -> expr |> read-mal |> (eval-mal env) |> print-mal

env = create-env BUILTINS
stdin.set-encoding \utf8
rl = readline.create-interface input: stdin, output: stdout
rl.set-prompt 'user> '
rl.on \line, (mal) ->
    if mal
        try
            ret = rep mal, env
            console.log(ret) if ret?
        catch e
            console.error e.stack
    rl.prompt()
rl.on \close, ->
    console.log '\nGoodbye!'
    process.exit!
rl.prompt()
