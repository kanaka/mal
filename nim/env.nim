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

proc set*(e: var Env, key: string, value: MalType): MalType {.discardable.} =
  e.data[key] = value
  value

proc find*(e: Env, key: string): Env =
  if e.data.hasKey(key): return e
  if e.outer != nil:     return e.outer.find(key)

proc get*(e: Env, key: string): MalType =
  let env = e.find(key)
  if env == nil: raise newException(ValueError, "'" & key & "' not found")
  env.data[key]
