require! {
    LiveScript
    readline
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
    'prelude-ls': {id}
}


{stdin, stdout} = process

read-mal = read-str

eval-mal = id

print-mal = (ast) ->
    if ast? then pr-str ast else null

rep = read-mal >> eval-mal >> print-mal

stdin.set-encoding \utf8
rl = readline.create-interface input: stdin, output: stdout
rl.set-prompt 'MAL > '
rl.on \line, (mal) ->
    ret = rep mal
    console.log(ret) if ret?
    rl.prompt()
rl.on \close, ->
    console.log '\nGoodbye!'
    process.exit!
rl.prompt()
