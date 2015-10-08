readline = require "./node_readline.coffee"
reader = require "./reader.coffee"
printer = require "./printer.coffee"

# read
READ = (str) -> reader.read_str str

# eval
EVAL = (ast, env) -> ast

# print
PRINT = (exp) -> printer._pr_str exp, true

# repl
rep = (str) -> PRINT(EVAL(READ(str), {}))

# repl loop
while (line = readline.readline("user> ")) != null
  continue if line == ""
  try
    console.log rep line
  catch exc
    continue if exc instanceof reader.BlankException
    if exc.stack? and exc.stack.length > 2000
      console.log exc.stack.slice(0,1000) + "\n  ..." + exc.stack.slice(-1000)
    else if exc.stack? console.log exc.stack
    else               console.log exc

# vim: ts=2:sw=2
