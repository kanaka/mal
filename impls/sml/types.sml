datatype mal_type = NIL
                  | SYMBOL of string
                  | BOOL of bool
                  | INT of int
                  | LIST of mal_type list
                  | FN of mal_type list -> mal_type

fun truthy (BOOL false) = false
  | truthy NIL          = false
  | truthy _            = true

fun malEq (     NIL, NIL)      = true
  | malEq (SYMBOL a, SYMBOL b) = a = b
  | malEq (  BOOL a, BOOL b)   = a = b
  | malEq (   INT a, INT b)    = a = b
  | malEq (  LIST a, LIST b)   = ListPair.allEq malEq (a, b)
  | malEq _                    = false
