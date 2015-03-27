import rdstdin, tables, sequtils, os, types, reader, printer, env, core

proc read(str: string): MalType = str.read_str

proc is_pair(x: MalType): bool =
  x.kind in {List, Vector} and x.list.len > 0

proc quasiquote(ast: MalType): MalType =
  if not ast.is_pair:
    return list(symbol "quote", ast)
  elif ast.list[0] == symbol "unquote":
    return ast.list[1]
  elif ast.list[0].is_pair and ast.list[0].list[0] == symbol "splice-unquote":
    return list(symbol "concat", ast.list[0].list[1],
      quasiquote(list ast.list[1 .. ^1]))
  else:
    return list(symbol "cons", quasiquote(ast.list[0]), quasiquote(list(ast.list[1 .. ^1])))

proc is_macro_call(ast: MalType, env: Env): bool =
  ast.kind == List and ast.list[0].kind == Symbol and
    env.find(ast.list[0].str) != nil and env.get(ast.list[0].str).macro_q

proc macroexpand(ast: MalType, env: Env): MalType =
  result = ast
  while result.is_macro_call(env):
    let mac = env.get(result.list[0].str)
    result = mac.malfun.fn(result.list[1 .. ^1]).macroexpand(env)

proc eval(ast: MalType, env: Env): MalType

proc eval_ast(ast: MalType, env: var Env): MalType =
  case ast.kind
  of Symbol:
    result = env.get(ast.str)
  of List:
    result = list ast.list.mapIt(MalType, it.eval(env))
  of Vector:
    result = vector ast.list.mapIt(MalType, it.eval(env))
  of HashMap:
    result = hash_map()
    for k, v in ast.hash_map.pairs:
      result.hash_map[k] = v.eval(env)
  else:
    result = ast

proc eval(ast: MalType, env: Env): MalType =
  var ast = ast
  var env = env

  template defaultApply =
    let el = ast.eval_ast(env)
    let f = el.list[0]
    case f.kind
    of MalFun:
      ast = f.malfun.ast
      env = initEnv(f.malfun.env, f.malfun.params, list(el.list[1 .. ^1]))
    else:
      return f.fun(el.list[1 .. ^1])

  while true:
    if ast.kind != List: return ast.eval_ast(env)

    ast = ast.macroexpand(env)
    if ast.kind != List: return ast
    if ast.list.len == 0: return ast

    let a0 = ast.list[0]
    case a0.kind
    of Symbol:
      case a0.str
      of "def!":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        return env.set(a1.str, a2.eval(env))

      of "let*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        var let_env = Env(env)
        case a1.kind
        of List, Vector:
          for i in countup(0, a1.list.high, 2):
            let_env.set(a1.list[i].str, a1.list[i+1].eval(let_env))
        else: raise newException(ValueError, "Illegal kind in let*")
        ast = a2
        env = let_env
        # Continue loop (TCO)

      of "quote":
        return ast.list[1]

      of "quasiquote":
        ast = ast.list[1].quasiquote
        # Continue loop (TCO)

      of "defmacro!":
        var fun = ast.list[2].eval(env)
        fun.malfun.is_macro = true
        return env.set(ast.list[1].str, fun)

      of "macroexpand":
        return ast.list[1].macroexpand(env)

      of "do":
        let last = ast.list.high
        let el = (list ast.list[1 .. <last]).eval_ast(env)
        ast = ast.list[last]
        # Continue loop (TCO)

      of "if":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
          cond = a1.eval(env)

        if cond.kind in {Nil, False}:
          if ast.list.len > 3: ast = ast.list[3]
          else: ast = nilObj
        else: ast = a2

      of "fn*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        var env2 = env
        let fn = proc(a: varargs[MalType]): MalType =
          var newEnv = initEnv(env2, a1, list(a))
          a2.eval(newEnv)
        return malfun(fn, a2, a1, env)

      else:
        defaultApply()

    else:
      defaultApply()

proc print(exp: MalType): string = exp.pr_str

var repl_env = initEnv()

for k, v in ns.items:
  repl_env.set(k, v)
repl_env.set("eval", fun(proc(xs: varargs[MalType]): MalType = eval(xs[0], repl_env)))
var ps = commandLineParams()
repl_env.set("*ARGV*", list((if paramCount() > 1: ps[1..ps.high] else: @[]).map(str)))


# core.nim: defined using nim
proc rep(str: string): string {.discardable.} =
  str.read.eval(repl_env).print

# core.mal: defined using mal itself
rep "(def! not (fn* (a) (if a false true)))"
rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
rep "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"

if paramCount() >= 1:
  rep "(load-file \"" & paramStr(1) & "\")"
  quit()

while true:
  try:
    let line = readLineFromStdin("user> ")
    echo line.rep
  except Blank: discard
  except:
    echo getCurrentExceptionMsg()
    echo getCurrentException().getStackTrace()
