import tables, types

proc initEnv*(outer: Env = nil, binds, exprs: MalType = nilObj): Env =
  result = Env(data: initTable[string, MalType](), outer: outer)

  if binds.kind in {List, Vector}:
    for i, e in binds.list:
      if e.str == "&":
        result.data[binds.list[i+1].str] = list(exprs.list[i .. ^1])
        break
      else:
        result.data[e.str] = exprs.list[i]

proc set*(e: Env, key: string, value: MalType): MalType {.discardable.} =
  e.data[key] = value
  value

proc get*(e: Env, key: string): MalType =
  var env = e
  while not env.data.hasKey(key):
    env = env.outer
    if env.isNil: return nil
  return env.data[key]
