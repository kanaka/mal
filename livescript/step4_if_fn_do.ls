require! {
    LiveScript
    readline
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

rep = (expr, env) -> expr |> read-mal |> (eval-mal env) |> print-mal

env = create-env core.ns
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
