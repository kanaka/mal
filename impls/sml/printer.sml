fun prStr NIL          = "nil"
  | prStr (SYMBOL s)   = s
  | prStr (BOOL true)  = "true"
  | prStr (BOOL false) = "false"
  | prStr (INT i)      = if i >= 0 then Int.toString i else "-" ^ (Int.toString (Int.abs i))
  | prStr (LIST l)     = "(" ^ (String.concatWith " " (map prStr l)) ^ ")" (* N.B. not tail recursive *)
  | prStr (FN f)       = "#<function>"
