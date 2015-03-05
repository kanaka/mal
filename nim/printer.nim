import strutils, sequtils, tables, types

proc str_handle(x: string, pr = true): string =
  if x.len > 0 and x[0] == '\xff':
            result = ":" & x[1 .. x.high]
  elif pr:  result = "\"" & x.replace("\\", "\\\\").replace("\"", "\\\"") & "\""
  else:     result = x

proc pr_str*(m: MalType, pr = true): string =
  case m.kind
  of Nil:     result = "nil"
  of True:    result = "true"
  of False:   result = "false"
  of Fun:     result = "#<function>"
  of MalFun:  result = "#<malfun>"
  of Atom:    result = "(atom " & m.val.pr_str & ")"
  of Symbol:  result = m.str
  of String:  result = m.str.str_handle(pr)
  of Number:  result = $m.number
  of List:    result = "(" & m.list.mapIt(string, it.pr_str(pr)).join(" ") & ")"
  of Vector:  result = "[" & m.list.mapIt(string, it.pr_str(pr)).join(" ") & "]"
  of HashMap:
    result = "{"
    for key, val in m.hash_map.pairs:
      if result.len > 1: result.add " "
      result.add key.str_handle & " " & val.pr_str(pr)
    result.add "}"
