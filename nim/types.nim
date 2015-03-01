import tables, strutils

type
  MalTypeKind* = enum Nil, True, False, Number, Symbol, String,
    List, Vector, HashMap, Fun, MalFun

  MalFunType* = ref object
    fn*:     proc(a: varargs[MalType]): MalType
    ast*:    MalType
    params*: MalType
    env*:    Env

  MalType* = object
    case kind*: MalTypeKind
    of Nil, True, False: nil
    of Number:           number*:   int
    of String, Symbol:   str*:      string
    of List, Vector:     list*:     seq[MalType]
    of HashMap:          hash_map*: TableRef[string, MalType]
    of Fun:              fun*:      proc(xs: varargs[MalType]): MalType
    of MalFun:           malfun*:   MalFunType

  Env* = ref object
    data*: Table[string, MalType]
    outer*: Env

# Convenience procs
const nilObj* = MalType(kind: Nil)
const trueObj* = MalType(kind: True)
const falseObj* = MalType(kind: False)

proc number*(x: int): MalType = MalType(kind: Number, number: x)

proc symbol*(x: string): MalType = MalType(kind: Symbol, str: x)

proc str*(x: string): MalType {.procvar.} = MalType(kind: String, str: x)

proc keyword*(x: string): MalType = MalType(kind: String, str: "\xff" & x)

proc list*(xs: varargs[MalType]): MalType {.procvar.} =
  result = MalType(kind: List, list: @[])
  for x in xs: result.list.add x

proc vector*(xs: varargs[MalType]): MalType {.procvar.} =
  result = MalType(kind: Vector, list: @[])
  for x in xs: result.list.add x

proc hash_map*(xs: varargs[MalType]): MalType {.procvar.} =
  result = MalType(kind: HashMap, hash_map: newTable[string, MalType]())
  for i in countup(0, xs.high, 2):
    let s = case xs[i].kind
    of String: "\"" & xs[i].str & "\""
    else: xs[i].str
    result.hash_map[s] = xs[i+1]

proc fun*(x: proc(xs: varargs[MalType]): MalType): MalType = MalType(kind: Fun, fun: x)

proc malfun*(fn: auto, ast, params: MalType,
  env: Env): MalType =
  MalType(kind: MalFun,
    malfun: MalFunType(fn: fn, ast: ast, params: params, env: env))

proc boolObj(b: bool): MalType =
  if b: trueObj else: falseObj

proc list_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == List

proc vector_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == Vector

proc hash_map_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == HashMap

proc empty_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].list.len == 0

proc count*(xs: varargs[MalType]): MalType {.procvar.} =
  number if xs[0].kind == Nil: 0 else: xs[0].list.len

proc `==`*(x, y: MalType): bool =
  if not (x.kind in {List, Vector} and y.kind in {List, Vector}):
    if x.kind != y.kind: return false
  result = case x.kind
  of Nil, True, False: true
  of Number:         x.number   == y.number
  of Symbol, String: x.str      == y.str
  of List, Vector:   x.list     == y.list
  of HashMap:        x.hash_map == y.hash_map
  of Fun:            x.fun      == y.fun
  of MalFun:         x.malfun   == y.malfun

proc equal*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0] == xs[1]
