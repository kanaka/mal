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
}
