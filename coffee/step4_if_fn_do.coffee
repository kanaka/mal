readline = require "./node_readline.coffee"
types = require "./types.coffee"
reader = require "./reader.coffee"
printer = require "./printer.coffee"
Env = require("./env.coffee").Env
core = require("./core.coffee")

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
    when "do"
      el = eval_ast(ast[1..], env)
      el[el.length-1]
    when "if"
      cond = EVAL(a1, env)
      if cond == null or cond == false
        if a3? then EVAL(a3, env) else null
      else
        EVAL(a2, env)
    when "fn*"
      (args...) -> EVAL(a2, new Env(env, a1, args))
    else
      [f, args...] = eval_ast ast, env
      f(args...)


# print
PRINT = (exp) -> printer._pr_str exp, true

# repl
repl_env = new Env()
rep = (str) -> PRINT(EVAL(READ(str), repl_env))

# core.coffee: defined using CoffeeScript
repl_env.set types._symbol(k), v for k,v of core.ns

# core.mal: defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");

# repl loop
while (line = readline.readline("user> ")) != null
  continue if line == ""
  try
    console.log rep line
  catch exc
    continue if exc instanceof reader.BlankException
    if exc.stack then console.log exc.stack
    else              console.log exc

# vim: ts=2:sw=2
