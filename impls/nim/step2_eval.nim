import rdstdin, tables, sequtils, types, reader, printer

proc read(str: string): MalType = str.read_str

proc eval(ast: MalType, env: Table[string, MalType]): MalType =

  # echo "EVAL: " & ast.pr_str

  case ast.kind
  of Symbol:
    if not env.hasKey(ast.str):
      raise newException(ValueError, "'" & ast.str & "' not found")
    result = env[ast.str]
  of Vector:
    result = vector ast.list.mapIt(it.eval(env))
  of HashMap:
    result = hash_map()
    for k, v in ast.hash_map.pairs:
      result.hash_map[k] = v.eval(env)
  of List:
    if ast.list.len == 0: return ast
    let el = ast.list.mapIt(it.eval(env))
    result = el[0].fun(el[1 .. ^1])
  else:
    result = ast

proc print(exp: MalType): string = exp.pr_str

template wrapNumberFun(op): untyped =
  fun proc(xs: varargs[MalType]): MalType = number op(xs[0].number, xs[1].number)

let repl_env = toTable({
  "+": wrapNumberFun `+`,
  "-": wrapNumberFun `-`,
  "*": wrapNumberFun `*`,
  "/": wrapNumberFun `div`,
})

proc rep(str: string): string =
  str.read.eval(repl_env).print

while true:
  try:
    let line = readLineFromStdin("user> ")
    echo line.rep
  except IOError: quit()
  except:
    echo getCurrentExceptionMsg()
