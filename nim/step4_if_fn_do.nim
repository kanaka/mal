import rdstdin, tables, sequtils, types, reader, printer, env, core

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
    if ast.list.len == 0: return ast
    let a0 = ast.list[0]
    case a0.kind
    of Symbol:
      case a0.str
      of "def!":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        result = env.set(a1.str, a2.eval(env))

      of "let*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        var letEnv: Env
        letEnv.deepCopy(env)

        case a1.kind
        of List, Vector:
          for i in countup(0, a1.list.high, 2):
            letEnv.set(a1.list[i].str, a1.list[i+1].eval(letEnv))
        else: discard
        result = a2.eval(letEnv)

      of "do":
        let el = (list ast.list[1 .. ^1]).eval_ast(env)
        result = el.list[el.list.high]

      of "if":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
          cond = a1.eval(env)

        if cond.kind in {Nil, False}:
          if ast.list.len > 3: result = ast.list[3].eval(env)
          else: result = nilObj
        else: result = a2.eval(env)

      of "fn*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        var env2 = env
        result = fun(proc(a: varargs[MalType]): MalType =
          var newEnv = initEnv(env2, a1, list(a))
          a2.eval(newEnv))

      else:
        let el = ast.eval_ast(env)
        result = el.list[0].fun(el.list[1 .. ^1])

    else:
      let el = ast.eval_ast(env)
      result = el.list[0].fun(el.list[1 .. ^1])

  else:
    result = ast.eval_ast(env)

proc print(exp: MalType): string = exp.pr_str

var repl_env = initEnv()

for k, v in ns.items:
  repl_env.set(k, v)

# core.nim: defined using nim
proc rep(str: string): string =
  str.read.eval(repl_env).print

# core.mal: defined using mal itself
discard rep "(def! not (fn* (a) (if a false true)))"

while true:
  try:
    let line = readLineFromStdin("user> ")
    echo line.rep
  except:
    echo getCurrentExceptionMsg()
    echo getCurrentException().getStackTrace()
