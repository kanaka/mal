import strutils, types, printer, reader

# String functions
proc pr_str(xs: varargs[MalType]): MalType =
  str(xs.map(proc(x: MalType): string = x.pr_str(true)).join(" ").replace("\\", "\\\\"))

proc do_str(xs: varargs[MalType]): MalType =
  str(xs.map(proc(x: MalType): string = x.pr_str(false)).join.replace("\\", "\\\\"))

proc prn(xs: varargs[MalType]): MalType =
  echo xs.map(proc(x: MalType): string = x.pr_str(true)).join(" ")
  result = nilObj

proc println(xs: varargs[MalType]): MalType =
  let line = xs.map(proc(x: MalType): string = x.pr_str(false)).join(" ")
  echo line.replace("\\n", "\n")
  result = nilObj

proc read_str(xs: varargs[MalType]): MalType =
  read_str(xs[0].str)

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
  if xs[0].list.len > 0: xs[0].list[0]
  else: nilObj

proc rest(xs: varargs[MalType]): MalType =
  if xs[0].list.len > 0: list xs[0].list[1 .. -1]
  else: list()

template wrapNumberFun(op: expr): expr =
  fun proc(xs: varargs[MalType]): MalType = number op(xs[0].number, xs[1].number)

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
  "hash_map": fun hash_map,
  "hash_map?": fun hash_map_q,
  "empty?": fun empty_q,
  "count": fun count,
  "=": fun equal,

  "pr-str": fun pr_str,
  "str": fun do_str,
  "prn": fun prn,
  "println": fun println,

  "read-string": fun read_str,
  "slurp": fun slurp,
  "cons": fun cons,
  "concat": fun concat,

  "nth": fun nth,
  "first": fun first,
  "rest": fun rest,
}
