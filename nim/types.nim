import tables, strutils

type
  MalTypeKind* = enum Nil, True, False, Number, Symbol, String,
    List, Vector, HashMap, Fun, MalFun, Atom

  FunType = proc(a: varargs[MalType]): MalType

  MalFunType* = ref object
    fn*:       FunType
    ast*:      MalType
    params*:   MalType
    env*:      Env
    is_macro*: bool

  MalType* = ref object
    case kind*: MalTypeKind
    of Nil, True, False: nil
    of Number:           number*:   int
    of String, Symbol:   str*:      string
    of List, Vector:     list*:     seq[MalType]
    of HashMap:          hash_map*: Table[string, MalType]
    of Fun:
                         fun*:      FunType
                         is_macro*: bool
    of MalFun:           malfun*:   MalFunType
    of Atom:             val*:      MalType

    meta*: MalType

  Env* = ref object
    data*: Table[string, MalType]
    outer*: Env

let nilObj* = MalType(kind: Nil)
let trueObj* = MalType(kind: True)
let falseObj* = MalType(kind: False)

proc number*(x: int): MalType = MalType(kind: Number, number: x)

proc symbol*(x: string): MalType = MalType(kind: Symbol, str: x)

proc str*(x: string): MalType {.procvar.} = MalType(kind: String, str: x)

proc keyword*(x: string): MalType = MalType(kind: String, str: "\xff" & x)

proc atom*(x: MalType): MalType =
  result = MalType(kind: Atom)
  result.val = x

proc list*(xs: varargs[MalType]): MalType {.procvar.} =
  result = MalType(kind: List, list: newSeq[MalType](xs.len))
  for i, x in xs: result.list[i] = x

proc vector*(xs: varargs[MalType]): MalType {.procvar.} =
  result = MalType(kind: Vector, list: newSeq[MalType](xs.len))
  for i, x in xs: result.list[i] = x

proc hash_map*(xs: varargs[MalType]): MalType {.procvar.} =
  result = MalType(kind: HashMap, hash_map: initTable[string, MalType]())
  for i in countup(0, xs.high, 2):
    let s = case xs[i].kind
    of String: xs[i].str
    else: xs[i].str
    result.hash_map[s] = xs[i+1]

proc macro_q*(x: MalType): bool =
  if x.kind == Fun: result = x.is_macro
  elif x.kind == MalFun: result = x.malfun.is_macro
  else: raise newException(ValueError, "no function")

proc getFun*(x: MalType): FunType =
  if x.kind == Fun: result = x.fun
  elif x.kind == MalFun: result = x.malfun.fn
  else: raise newException(ValueError, "no function")

proc fun*(x: proc(xs: varargs[MalType]): MalType, is_macro = false): MalType =
  MalType(kind: Fun, fun: x, is_macro: is_macro)

proc malfun*(fn: auto, ast, params: MalType,
             env: Env, is_macro = false): MalType =
  MalType(kind: MalFun, malfun: MalFunType(fn: fn, ast: ast, params: params,
    env: env, is_macro: is_macro))

proc boolObj*(b: bool): MalType =
  if b: trueObj else: falseObj

proc list_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == List

proc vector_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == Vector

proc seq_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind in {List, Vector}

proc hash_map_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == HashMap

proc empty_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].list.len == 0

proc nil_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == Nil

proc true_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == True

proc false_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == False

proc symbol*(xs: varargs[MalType]): MalType {.procvar.} =
  symbol(xs[0].str)

proc symbol_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == Symbol

proc keyword*(xs: varargs[MalType]): MalType {.procvar.} =
  keyword(xs[0].str)

proc keyword_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj(xs[0].kind == String and xs[0].str[0] == '\xff')

proc atom*(xs: varargs[MalType]): MalType {.procvar.} =
  atom(xs[0])

proc atom_q*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0].kind == Atom

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
  of Fun:            x.fun      == y.fun and
                     x.is_macro == y.is_macro
  of MalFun:         x.malfun   == y.malfun
  of Atom:           x.val      == y.val

proc equal*(xs: varargs[MalType]): MalType {.procvar.} =
  boolObj xs[0] == xs[1]
