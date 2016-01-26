readline = require "./node_readline.coffee"
types = require "./types.coffee"
reader = require "./reader.coffee"
printer = require "./printer.coffee"
Env = require("./env.coffee").Env
core = require("./core.coffee")

# read
READ = (str) -> reader.read_str str

# eval
is_pair = (x) -> types._sequential_Q(x) && x.length > 0

quasiquote = (ast) ->
  if !is_pair(ast) then [types._symbol('quote'), ast]
  else if ast[0] != null && ast[0].name == 'unquote' then ast[1]
  else if is_pair(ast[0]) && ast[0][0].name == 'splice-unquote'
    [types._symbol('concat'), ast[0][1], quasiquote(ast[1..])]
  else
    [types._symbol('cons'), quasiquote(ast[0]), quasiquote(ast[1..])]

is_macro_call = (ast, env) ->
  return types._list_Q(ast) && types._symbol_Q(ast[0]) &&
         env.find(ast[0]) && env.get(ast[0]).__ismacro__

macroexpand = (ast, env) ->
  while is_macro_call(ast, env)
    ast = env.get(ast[0])(ast[1..]...)
  ast
    
    

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

  # apply list
  ast = macroexpand ast, env
  if !types._list_Q ast then return eval_ast ast, env

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
    when "quote"
      return a1
    when "quasiquote"
      ast = quasiquote(a1)
    when "defmacro!"
      f = EVAL(a2, env)
      f.__ismacro__ = true
      return env.set(a1, f)
    when "macroexpand"
      return macroexpand(a1, env)
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
repl_env.set types._symbol('eval'), (ast) -> EVAL(ast, repl_env)
repl_env.set types._symbol('*ARGV*'), []

# core.mal: defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")

if process? && process.argv.length > 2
  repl_env.set types._symbol('*ARGV*'), process.argv[3..]
  rep('(load-file "' + process.argv[2] + '")')
  process.exit 0

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
