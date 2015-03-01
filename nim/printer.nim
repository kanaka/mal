import strutils, sequtils, tables, types

proc pr_str*(m: MalType, pr = true): string =
  case m.kind
  of Nil:     result = "nil"
  of True:    result = "true"
  of False:   result = "false"
  of Fun:     result = "#<function>"
  of MalFun:  result = "#<malfun>"
  of Symbol:  result = m.str
  of String:
    if m.str.len > 0 and m.str[0] == '\xff':
              result = ":" & m.str[1 .. m.str.high]
    elif pr:  result = "\"" & m.str.replace("\"", "\\\"") & "\""
    else:     result = m.str
  of Number:  result = $m.number
  of List:    result = "(" & m.list.mapIt(string, it.pr_str(pr)).join(" ") & ")"
  of Vector:  result = "[" & m.list.mapIt(string, it.pr_str(pr)).join(" ") & "]"
  of HashMap:
    result = "{"
    for key, val in m.hash_map.pairs:
      if result.len > 1: result.add " "
      result.add key & " " & val.pr_str(pr)
    result.add "}"
