readline = require 'readline'
{id} = require 'prelude-ls'


READ = id
EVAL = ->
PRINT = id
rep = (line) ->


rl = readline.createInterface do
    input : process.stdin
    output : process.stdout
    prompt: 'user> '

rl.on 'line', (line) ->
    console.log rep line
    rl.prompt!

rl.on 'close', ->
    process.exit 0

rl.prompt!
