import rdstdin, tables, sequtils, types, reader, printer, env, core

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

    let a0 = ast.list[0]
    if a0.kind == Symbol:
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
        let let_env = initEnv(env)
        case a1.kind
        of List, Vector:
          for i in countup(0, a1.list.high, 2):
            let_env.set(a1.list[i].str, a1.list[i+1].eval(let_env))
        else: discard
        return a2.eval(let_env)

      of "do":
        let el = ast.list[1 .. ^1].mapIt(it.eval(env))
        return el[el.high]

      of "if":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
          cond = a1.eval(env)

        if cond.kind in {Nil, False}:
          if ast.list.len > 3: return ast.list[3].eval(env)
          else: return nilObj
        else: return a2.eval(env)

      of "fn*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        return fun(proc(a: varargs[MalType]): MalType =
          a2.eval(initEnv(env, a1, list(a))))

    let el = ast.list.mapIt(it.eval(env))
    result = el[0].fun(el[1 .. ^1])

  else:
    result = ast

proc print(exp: MalType): string = exp.pr_str

let repl_env = initEnv()

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
  except IOError: quit()
  except:
    echo getCurrentExceptionMsg()
    echo getCurrentException().getStackTrace()
