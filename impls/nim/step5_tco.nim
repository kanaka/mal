import rdstdin, tables, sequtils, types, reader, printer, env, core

proc read(str: string): MalType = str.read_str

proc eval(ast: MalType, env: Env): MalType =
  var ast = ast
  var env = env

  while true:

    let dbgeval = env.get("DEBUG-EVAL")
    if not (dbgeval.isNil or dbgeval.kind in {Nil, False}):
      echo "EVAL: " & ast.pr_str

    case ast.kind
    of Symbol:
      let val = env.get(ast.str)
      if val.isNil:
        raise newException(ValueError, "'" & ast.str & "' not found")
      return val
    of List:
      discard(nil) # Proceed after the case statement
    of Vector:
      return vector ast.list.mapIt(it.eval(env))
    of HashMap:
      result = hash_map()
      for k, v in ast.hash_map.pairs:
        result.hash_map[k] = v.eval(env)
      return result
    else:
      return ast
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
        else: raise newException(ValueError, "Illegal kind in let*")
        ast = a2
        env = let_env
        continue # TCO

      of "do":
        let last = ast.list.high
        discard (ast.list[1 ..< last].mapIt(it.eval(env)))
        ast = ast.list[last]
        continue # TCO

      of "if":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
          cond = a1.eval(env)

        if cond.kind in {Nil, False}:
          if ast.list.len > 3:
            ast = ast.list[3]
            continue # TCO
          else:
            return nilObj
        else:
          ast = a2
          continue # TCO

      of "fn*":
        let
          a1 = ast.list[1]
          a2 = ast.list[2]
        let fn = proc(a: varargs[MalType]): MalType =
          a2.eval(initEnv(env, a1, list(a)))
        return malfun(fn, a2, a1, env)

    let f = eval(a0, env)
    let args = ast.list[1 .. ^1].mapIt(it.eval(env))
    if f.kind == MalFun:
      ast = f.malfun.ast
      env = initEnv(f.malfun.env, f.malfun.params, list(args))
      continue # TCO

    return f.fun(args)

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
