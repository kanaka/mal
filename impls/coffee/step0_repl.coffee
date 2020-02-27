readline = require "./node_readline.coffee"

# read
READ = (str) -> str

# eval
EVAL = (ast, env) -> ast

# print
PRINT = (exp) -> exp

# repl
rep = (str) -> PRINT(EVAL(READ(str), {}))

# repl loop
while (line = readline.readline("user> ")) != null
  continue if line == ""
  console.log rep line

# vim: ts=2:sw=2
