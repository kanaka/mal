fun prStr NIL          = "nil"
  | prStr (SYMBOL s)   = s
  | prStr (BOOL true)  = "true"
  | prStr (BOOL false) = "false"
  | prStr (ATOM x)     = "#<atom> (" ^ (prStr (!x)) ^ ")"
  | prStr (INT i)      = if i >= 0 then Int.toString i else "-" ^ (Int.toString (Int.abs i))
  | prStr (STRING s)   = s
  | prStr (LIST l)     = "(" ^ (String.concatWith " " (map prStr l)) ^ ")" (* N.B. not tail recursive *)
  | prStr (FN _)       = "#<function>"
  | prStr (CLOSURE _)  = "#<function>"

fun prReadableStr (STRING s) = "\"" ^ (malEscape s) ^ "\""
  | prReadableStr (ATOM x)   = "(atom " ^ (prReadableStr (!x)) ^ ")"
  | prReadableStr (LIST l)   = "(" ^ (String.concatWith " " (map prReadableStr l)) ^ ")" (* N.B. not tail recursive *)
  | prReadableStr x          = prStr x
