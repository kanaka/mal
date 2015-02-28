import strutils, tables, types

proc pr_str*(m: MalType): string =
  case m.kind
  of Nil:     result = "nil"
  of Fun:     result = "fun"
  of Symbol:  result = m.symbol
  of Number:  result = $m.number
  of List:    result = "(" & m.list.map(pr_str).join(" ") & ")"
  of Vector:  result = "[" & m.vector.map(pr_str).join(" ") & "]"
  of HashMap:
    result = "{"
    for key, val in m.hash_map.pairs:
      if result.len > 1: result.add " "
      result.add key & " " & val.pr_str
    result.add "}"
