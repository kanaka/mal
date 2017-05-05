readline = require './node_readline'
{id} = require 'prelude-ls'
{read_str} = require './reader'
{pr_str} = require './printer'


EVAL = id

rep = (line) -> pr_str EVAL read_str line

loop
    line = readline.readline 'user> '
    break if not line? or line == ''
    try
        console.log rep line
    catch {message}
        console.log message
