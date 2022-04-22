fun prStr NIL            = "nil"
  | prStr (SYMBOL s)     = s
  | prStr (BOOL true)    = "true"
  | prStr (BOOL false)   = "false"
  | prStr (ATOM x)       = "#<atom> (" ^ (prStr (!x)) ^ ")"
  | prStr (INT i)        = if i >= 0 then LargeInt.toString i else "-" ^ (LargeInt.toString (LargeInt.abs i))
  | prStr (STRING s)     = s
  | prStr (KEYWORD s)    = ":" ^ s
  | prStr (LIST (l,_))   = "(" ^ (String.concatWith " " (map prStr l)) ^ ")" (* N.B. not tail recursive *)
  | prStr (VECTOR (v,_)) = "[" ^ (String.concatWith " " (map prStr v)) ^ "]" (* N.B. not tail recursive *)
  | prStr (MAP (m,_))    = "{" ^ (String.concatWith " " (map prKvp m)) ^ "}" (* N.B. not tail recursive *)
  | prStr (FN _)         = "#<function>"
  | prStr (MACRO _)      = "#<macro>"
and prKvp (k, v) = (prStr k) ^ " " ^ (prStr v)

fun prReadableStr (STRING s)     = "\"" ^ (malEscape s) ^ "\""
  | prReadableStr (ATOM x)       = "(atom " ^ (prReadableStr (!x)) ^ ")"
  | prReadableStr (LIST (l,_))   = "(" ^ (String.concatWith " " (map prReadableStr l)) ^ ")" (* N.B. not tail recursive *)
  | prReadableStr (VECTOR (v,_)) = "[" ^ (String.concatWith " " (map prReadableStr v)) ^ "]" (* N.B. not tail recursive *)
  | prReadableStr (MAP (m,_))    = "{" ^ (String.concatWith " " (map prReadableKvp m)) ^ "}" (* N.B. not tail recursive *)
  | prReadableStr x              = prStr x
and prReadableKvp (k, v) = (prReadableStr k) ^ " " ^ (prReadableStr v)
