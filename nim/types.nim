import tables

type
  MalTypeKind* = enum Nil, Number, Symbol, List, Vector, HashMap

  MalType* = object
    case kind*: MalTypeKind
    of Nil:     nil
    of Number:  number*:   int
    of Symbol:  symbol*:   string
    of List:    list*:     seq[MalType]
    of Vector:  vector*:   seq[MalType]
    of HashMap: hash_map*: TableRef[string, MalType]

# Convenience procs
const nilObj*: MalType = MalType(kind: Nil)

proc number*(x: int): MalType = MalType(kind: Number, number: x)

proc symbol*(x: string): MalType = MalType(kind: Symbol, symbol: x)

proc list*(xs: varargs[MalType]): MalType =
  result = MalType(kind: List, list: @[])
  for x in xs: result.list.add x

proc vector*(xs: varargs[MalType]): MalType =
  result = MalType(kind: Vector, vector: @[])
  for x in xs: result.vector.add x

proc hash_map*(xs: varargs[MalType]): MalType =
  result = MalType(kind: HashMap, hash_map: newTable[string, MalType]())
  for i in countup(0, xs.high, 2):
    result.hash_map[xs[i].symbol] = xs[i+1]
