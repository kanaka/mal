readline = require "./node_readline.coffee"
types = require "./types.coffee"
reader = require "./reader.coffee"
printer = require "./printer.coffee"
Env = require("./env.coffee").Env

# read
READ = (str) -> reader.read_str str

# eval
eval_ast = (ast, env) ->
  if types._symbol_Q(ast) then env.get ast
  else if types._list_Q(ast) then ast.map((a) -> EVAL(a, env))
  else if types._vector_Q(ast)
    types._vector(ast.map((a) -> EVAL(a, env))...)
  else if types._hash_map_Q(ast)
    new_hm = {}
    new_hm[k] = EVAL(ast[k],env) for k,v of ast
    new_hm
  else ast

EVAL = (ast, env) ->
  #console.log "EVAL:", printer._pr_str ast
  if !types._list_Q ast then return eval_ast ast, env

  # apply list
  [a0, a1, a2, a3] = ast
  switch a0.name
    when "def!"
      env.set(a1, EVAL(a2, env))
    when "let*"
      let_env = new Env(env)
      for k,i in a1 when i %% 2 == 0
        let_env.set(a1[i], EVAL(a1[i+1], let_env))
      EVAL(a2, let_env)
    else
      [f, args...] = eval_ast ast, env
      f(args...)


# print
PRINT = (exp) -> printer._pr_str exp, true

# repl
repl_env = new Env()
rep = (str) -> PRINT(EVAL(READ(str), repl_env))

repl_env.set types._symbol("+"), (a,b) -> a+b
repl_env.set types._symbol("-"), (a,b) -> a-b
repl_env.set types._symbol("*"), (a,b) -> a*b
repl_env.set types._symbol("/"), (a,b) -> a/b

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
