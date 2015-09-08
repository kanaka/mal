import strutils, rdstdin, tables, algorithm, times, types, printer, reader

type MalError* = object of Exception
  t*: MalType

# String functions
proc pr_str(xs: varargs[MalType]): MalType =
  str(xs.map(proc(x: MalType): string = x.pr_str(true)).join(" "))

proc do_str(xs: varargs[MalType]): MalType =
  str(xs.map(proc(x: MalType): string = x.pr_str(false)).join)

proc prn(xs: varargs[MalType]): MalType =
  echo xs.map(proc(x: MalType): string = x.pr_str(true)).join(" ")
  result = nilObj

proc println(xs: varargs[MalType]): MalType =
  let line = xs.map(proc(x: MalType): string = x.pr_str(false)).join(" ")
  echo line.replace("\\n", "\n")
  result = nilObj

proc read_str(xs: varargs[MalType]): MalType =
  read_str(xs[0].str)

proc readline(xs: varargs[MalType]): MalType =
  str readLineFromStdin(xs[0].str)

proc slurp(xs: varargs[MalType]): MalType =
  str readFile(xs[0].str)

proc cons(xs: varargs[MalType]): MalType =
  result = list(xs[0])
  for x in xs[1].list: result.list.add x

proc concat(xs: varargs[MalType]): MalType =
  result = list()
  for x in xs:
    for i in x.list:
      result.list.add i

proc nth(xs: varargs[MalType]): MalType =
  if xs[1].number < xs[0].list.len: return xs[0].list[xs[1].number]
  else: raise newException(ValueError, "nth: index out of range")

proc first(xs: varargs[MalType]): MalType =
  if xs[0].kind in {List, Vector} and xs[0].list.len > 0:
    xs[0].list[0]
  else: nilObj

proc rest(xs: varargs[MalType]): MalType =
  if xs[0].kind in {List, Vector} and xs[0].list.len > 0:
    list xs[0].list[1 .. ^1]
  else: list()

proc throw(xs: varargs[MalType]): MalType =
  raise (ref MalError)(t: list xs)

proc assoc(xs: varargs[MalType]): MalType =
  result = hash_map()
  result.hash_map = xs[0].hash_map
  for i in countup(1, xs.high, 2):
    result.hash_map[xs[i].str] = xs[i+1]

proc dissoc(xs: varargs[MalType]): MalType =
  result = hash_map()
  result.hash_map = xs[0].hash_map
  for i in 1 .. xs.high:
    if result.hash_map.hasKey(xs[i].str): result.hash_map.del(xs[i].str)

proc get(xs: varargs[MalType]): MalType =
  if xs[0].kind == HashMap:
    result = xs[0].hash_map[xs[1].str]
    if not result.isNil: return

  result = nilObj

proc contains_q(xs: varargs[MalType]): MalType =
  boolObj xs[0].hash_map.hasKey(xs[1].str)

proc keys(xs: varargs[MalType]): MalType =
  result = list()
  for key in xs[0].hash_map.keys:
    result.list.add str(key)

proc vals(xs: varargs[MalType]): MalType =
  result = list()
  for value in xs[0].hash_map.values:
    result.list.add value

proc conj(xs: varargs[MalType]): MalType =
  if xs[0].kind == List:
    result = list()
    for i in countdown(xs.high, 1):
      result.list.add xs[i]
    result.list.add xs[0].list
  else:
    result = vector()
    result.list.add xs[0].list
    for i in 1..xs.high:
      result.list.add xs[i]
  result.meta = xs[0].meta

proc apply(xs: varargs[MalType]): MalType =
  var s = newSeq[MalType]()
  if xs.len > 2:
    for j in 1 .. xs.high-1:
      s.add xs[j]
  s.add xs[xs.high].list
  xs[0].getFun()(s)

proc map(xs: varargs[MalType]): MalType =
  result = list()
  for i in 0 .. xs[1].list.high:
    result.list.add xs[0].getFun()(xs[1].list[i])

proc with_meta(xs: varargs[MalType]): MalType =
  new result
  result[] = xs[0][]
  result.meta = xs[1]

proc meta(xs: varargs[MalType]): MalType =
  if not xs[0].meta.isNil: xs[0].meta
  else: nilObj

proc deref(xs: varargs[MalType]): MalType =
  xs[0].val

proc reset_bang(xs: varargs[MalType]): MalType =
  xs[0].val = xs[1]
  result = xs[0].val

proc swap_bang(xs: varargs[MalType]): MalType =
  var args = @[xs[0].val]
  for i in 2 .. xs.high:
    args.add xs[i]
  xs[0].val = xs[1].getFun()(args)
  result = xs[0].val

proc time_ms(xs: varargs[MalType]): MalType =
  number int(epochTime() * 1000)

template wrapNumberFun(op: expr): expr =
  fun proc(xs: varargs[MalType]): MalType =
    number op(xs[0].number, xs[1].number)

template wrapBoolFun(op: expr): expr =
  fun proc(xs: varargs[MalType]): MalType =
    if op(xs[0].number, xs[1].number): trueObj else: falseObj

let ns* = {
  "+": wrapNumberFun(`+`),
  "-": wrapNumberFun(`-`),
  "*": wrapNumberFun(`*`),
  "/": wrapNumberFun(`div`),

  "<":  wrapBoolFun(`<`),
  "<=": wrapBoolFun(`<=`),
  ">":  wrapBoolFun(`>`),
  ">=": wrapBoolFun(`>=`),

  "list": fun list,
  "list?": fun list_q,
  "vector": fun vector,
  "vector?": fun vector_q,
  "hash-map": fun hash_map,
  "map?": fun hash_map_q,
  "empty?": fun empty_q,
  "assoc": fun assoc,
  "dissoc": fun dissoc,
  "get": fun get,
  "contains?": fun contains_q,
  "keys": fun keys,
  "vals": fun vals,

  "=": fun equal,

  "pr-str": fun pr_str,
  "str": fun do_str,
  "prn": fun prn,
  "println": fun println,

  "read-string": fun read_str,
  "readline": fun readline,
  "slurp": fun slurp,

  "sequential?": fun seq_q,
  "cons": fun cons,
  "concat": fun concat,
  "count": fun count,
  "nth": fun nth,
  "first": fun first,
  "rest": fun rest,
  "conj": fun conj,
  "apply": fun apply,
  "map": fun map,

  "throw": fun throw,

  "nil?": fun nil_q,
  "true?": fun true_q,
  "false?": fun false_q,
  "symbol": fun symbol,
  "symbol?": fun symbol_q,
  "keyword": fun keyword,
  "keyword?": fun keyword_q,

  "with-meta": fun with_meta,
  "meta": fun meta,
  "atom": fun atom,
  "atom?": fun atom_q,
  "deref": fun deref,
  "reset!": fun reset_bang,
  "swap!": fun swap_bang,

  "time-ms": fun time_ms,
}
