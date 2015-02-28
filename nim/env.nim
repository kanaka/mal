import tables, types

type Env* = ref object
  data: Table[string, MalType]
  outer: Env

proc initEnv*: Env = Env(data: initTable[string, MalType]())

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
