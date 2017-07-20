#ifndef __printer__
#define __printer__

#include "types.rexx"

format_string: procedure /* format_string(str, readable) */
  str = arg(1)
  readable = arg(2)
  if readable then do
    res = changestr('5C'x, str, "\\")
    res = changestr('"', res, '\"')
    res = changestr('0A'x, res, "\n")
    return '"' || res || '"'
  end
  else
    return str

format_sequence: procedure expose values. /* format_sequence(val, open_char, close_char, readable) */
  val = arg(1)
  open_char = arg(2)
  close_char = arg(3)
  readable = arg(4)
  res = ""
  do i=1 to words(val)
    element = word(val, i)
    if i > 1 then res = res || " "
    res = res || pr_str(element, readable)
  end
  return open_char || res || close_char

pr_str: procedure expose values. /* pr_str(ast, readable) */
  ast = arg(1)
  readable = arg(2)
  type = obj_type(ast)
  val = obj_val(ast)
  select
    when type == "nill" then return "nil"
    when type == "true" then return "true"
    when type == "fals" then return "false"
    when type == "numb" then return val
    when type == "symb" then return val
    when type == "stri" then return format_string(val, readable)
    when type == "keyw" then return ":" || val
    when type == "list" then return format_sequence(val, "(", ")", readable)
    when type == "vect" then return format_sequence(val, "[", "]", readable)
    when type == "hash" then return format_sequence(val, "{", "}", readable)
    when type == "nafn" then return "#<nativefunction:" || val || ">"
    when type == "func" then return "#<function:args=" || pr_str(func_binds(ast), readable) || ">"
    when type == "atom" then return "(atom " || pr_str(val, readable) || ")"
    otherwise
      return "#<UNKNOWN-TYPE>"
    end

#endif
