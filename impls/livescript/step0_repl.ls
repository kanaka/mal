readline = require './node_readline'
{id} = require 'prelude-ls'


READ = id
EVAL = id
PRINT = id

rep = (line) -> PRINT EVAL READ line

loop
  line = readline.readline 'user> '
  break if not line? or line == ''
  console.log rep line

# rl = readline.createInterface do
#     input : process.stdin
#     output : process.stdout
#     prompt: 'user> '

# rl.prompt!

# rl.on 'line', (line) ->
#     console.log rep line
#     rl.prompt!

# rl.on 'close', ->
#     process.exit 0
