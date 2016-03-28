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
 loop
  #console.log "EVAL:", printer._pr_str ast
  if !types._list_Q ast then return eval_ast ast, env
  if ast.length == 0 then return ast

  # apply list
  [a0, a1, a2, a3] = ast
  switch a0.name
    when "def!"
      return env.set(a1, EVAL(a2, env))
    when "let*"
      let_env = new Env(env)
      for k,i in a1 when i %% 2 == 0
        let_env.set(a1[i], EVAL(a1[i+1], let_env))
      ast = a2
      env = let_env
    when "do"
      eval_ast(ast[1..-2], env)
      ast = ast[ast.length-1]
    when "if"
      cond = EVAL(a1, env)
      if cond == null or cond == false
        if a3? then ast = a3 else return null
      else
        ast = a2
    when "fn*"
      return types._function(EVAL, a2, env, a1)
    else
      [f, args...] = eval_ast ast, env
      if types._function_Q(f)
        ast = f.__ast__
        env = f.__gen_env__(args)
      else
        return f(args...)


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
    if exc.stack? and exc.stack.length > 2000
      console.log exc.stack.slice(0,1000) + "\n  ..." + exc.stack.slice(-1000)
    else if exc.stack? console.log exc.stack
    else               console.log exc

# vim: ts=2:sw=2
