readline = require "./node_readline.coffee"
types = require "./types.coffee"
reader = require "./reader.coffee"
printer = require "./printer.coffee"

# read
READ = (str) -> reader.read_str str

# eval
EVAL = (ast, env) ->
  # console.log "EVAL:", printer._pr_str ast

  if types._symbol_Q(ast) then return env[ast.name]
  else if types._list_Q(ast) then # exit this switch
  else if types._list_Q(ast) then # exit this switch
  else if types._vector_Q(ast)
    return types._vector(ast.map((a) -> EVAL(a, env))...)
  else if types._hash_map_Q(ast)
    new_hm = {}
    new_hm[k] = EVAL(v, env) for k,v of ast
    return new_hm
  else return ast

  if ast.length == 0 then return ast

  # apply list
  [f, args...] = ast.map((a) -> EVAL(a, env))
  f(args...)


# print
PRINT = (exp) -> printer._pr_str exp, true

# repl
repl_env = {}
rep = (str) -> PRINT(EVAL(READ(str), repl_env))

repl_env["+"] = (a,b) -> a+b
repl_env["-"] = (a,b) -> a-b
repl_env["*"] = (a,b) -> a*b
repl_env["/"] = (a,b) -> a/b

# repl loop
while (line = readline.readline("user> ")) != null
  continue if line == ""
  try
    console.log rep line
  catch exc
    continue if exc instanceof reader.BlankException
    if exc.stack? and exc.stack.length > 2000
      console.log exc.stack.slice(0,1000) + "\n  ..." + exc.stack.slice(-1000)
    else if exc.stack? then console.log exc.stack
    else                    console.log exc

# vim: ts=2:sw=2
