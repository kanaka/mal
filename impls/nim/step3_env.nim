import rdstdin, tables, sequtils, types, reader, printer, env

proc read(str: string): MalType = str.read_str

proc eval(ast: MalType, env: Env): MalType =

  let dbgeval = env.get("DEBUG-EVAL")
  if not (dbgeval.isNil or dbgeval.kind in {Nil, False}):
    echo "EVAL: " & ast.pr_str

  case ast.kind
  of Symbol:
    result = env.get(ast.str)
    if result.isNil:
      raise newException(ValueError, "'" & ast.str & "' not found")
  of Vector:
    result = vector ast.list.mapIt(it.eval(env))
  of HashMap:
    result = hash_map()
    for k, v in ast.hash_map.pairs:
      result.hash_map[k] = v.eval(env)
  of List:
    if ast.list.len == 0: return ast
    let
      a0 = ast.list[0]
      a1 = ast.list[1]
      a2 = ast.list[2]

    case a0.str
    of "def!":
      result = env.set(a1.str, a2.eval(env))
    of "let*":
      let let_env = initEnv(env)
      case a1.kind
      of List, Vector:
        for i in countup(0, a1.list.high, 2):
          let_env.set(a1.list[i].str, a1.list[i+1].eval(let_env))
      else: discard
      result = a2.eval(let_env)
    else:
      let el = ast.list.mapIt(it.eval(env))
      result = el[0].fun(el[1 .. ^1])
  else:
    result = ast

proc print(exp: MalType): string = exp.pr_str

template wrapNumberFun(op): untyped =
  fun proc(xs: varargs[MalType]): MalType = number op(xs[0].number, xs[1].number)

let repl_env = initEnv()

repl_env.set("+", wrapNumberFun(`+`))
repl_env.set("-", wrapNumberFun(`-`))
repl_env.set("*", wrapNumberFun(`*`))
repl_env.set("/", wrapNumberFun(`div`))
#repl_env.set("/", wrapNumberFun(proc(x,y: int): int = int(x.float / y.float)))

proc rep(str: string): string =
  str.read.eval(repl_env).print

while true:
  try:
    let line = readLineFromStdin("user> ")
    echo line.rep
  except IOError: quit()
  except:
    echo getCurrentExceptionMsg()
    echo getCurrentException().getStackTrace()
