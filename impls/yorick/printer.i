require, "types.i"

func format_seq(val, start_char, end_char, readable)
{
  seq = *val
  res = ""
  for (i = 1; i <= numberof(seq); ++i) {
    if (i > 1) res += " "
    res += pr_str(*seq(i), readable)
  }
  return start_char + res + end_char
}

func format_hashmap(h, readable)
{
  res = ""
  for (i = 1; i <= numberof(*h.keys); ++i) {
    if (i > 1) res += " "
    key = hashmap_key_to_obj((*h.keys)(i))
    res += pr_str(key, readable) + " " + pr_str(*((*h.vals)(i)), readable)
  }
  return "{" + res + "}"
}

func escape(s)
{
  s1 = streplaceall(s, "\\", "\\\\")
  s2 = streplaceall(s1, "\"", "\\\"")
  s3 = streplaceall(s2, "\n", "\\n")
  return "\"" + s3 + "\""
}

func pr_str(ast, readable)
{
  type = structof(ast)
  if (type == MalNil) return "nil"
  else if (type == MalTrue) return "true"
  else if (type == MalFalse) return "false"
  else if (type == MalNumber) return totxt(ast.val)
  else if (type == MalSymbol) return ast.val
  else if (type == MalString) return readable ? escape(ast.val) : ast.val
  else if (type == MalKeyword) return ":" + ast.val
  else if (type == MalList) return format_seq(ast.val, "(", ")", readable)
  else if (type == MalVector) return format_seq(ast.val, "[", "]", readable)
  else if (type == MalHashmap) return format_hashmap(*ast.val, readable)
  else if (type == MalAtom) return "(atom " + pr_str(*(ast.val->val), readable) + ")"
  else if (type == MalNativeFunction) return "#<nativefunction:" + ast.val + ">"
  else if (type == MalFunction) return "#<function:" + totxt(numberof(*ast.binds)) + " args>"
  else MalError(message=("Unknown type " + totxt(type)))
}
