import tables, types

type Env* = ref object
  data: Table[string, MalType]
  outer: Env

proc initEnv*(outer: Env = nil, binds, exprs: MalType = nilObj): Env =
  result = Env(data: initTable[string, MalType](), outer: outer)

  if binds.kind in {List, Vector}:
    for i, e in binds.list:
      if e.symbol == "&":
        result.data[binds.list[i+1].symbol] = list(exprs.list[i .. exprs.list.high])
        break
      else:
        result.data[e.symbol] = exprs.list[i]

proc set*(e: var Env, key: string, value: MalType): MalType {.discardable.} =
  e.data[key] = value
  value

proc find(e: Env, key: string): Env =
  if e.data.hasKey(key): return e
  if e.outer != nil:     return e.outer.find(key)

proc get*(e: Env, key: string): MalType =
  let env = e.find(key)
  if env == nil: raise newException(ValueError, "'" & key & "' not found")
  env.data[key]
