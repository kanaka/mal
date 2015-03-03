require! {
    readline
    'prelude-ls': {id}
}
{stdin, stdout} = process

read-mal = id

eval-mal = id

print-mal = id

rep = print-mal << eval-mal << print-mal

rl = readline.create-interface input: stdin, output: stdout
rl.set-prompt 'user> '
rl.on \line, (mal) ->
    console.log rep mal
    rl.prompt()
rl.on \close, ->
    console.log '\nGoodbye!'
    process.exit!
rl.prompt()
