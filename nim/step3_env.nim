import rdstdin, tables, sequtils, types, reader, printer, env

proc read(str: string): MalType = str.read_str

proc eval(ast: MalType, env: var Env): MalType

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

proc eval(ast: MalType, env: var Env): MalType =
  case ast.kind
  of List:
    let
      a0 = ast.list[0]
      a1 = ast.list[1]
      a2 = ast.list[2]

    case a0.str
    of "def!":
      result = env.set(a1.str, a2.eval(env))
    of "let*":
      var letEnv: Env
      letEnv.deepCopy(env)
      case a1.kind
      of List, Vector:
        for i in countup(0, a1.list.high, 2):
          letEnv.set(a1.list[i].str, a1.list[i+1].eval(letEnv))
      else: discard
      result = a2.eval(letEnv)
    else:
      let el = ast.eval_ast(env)
      result = el.list[0].fun(el.list[1 .. ^1])
  else:
    result = ast.eval_ast(env)

proc print(exp: MalType): string = exp.pr_str

template wrapNumberFun(op: expr): expr =
  fun proc(xs: varargs[MalType]): MalType = number op(xs[0].number, xs[1].number)

var repl_env = initEnv()

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
  except:
    echo getCurrentExceptionMsg()
    echo getCurrentException().getStackTrace()
