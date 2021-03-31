datatype mal_type = NIL
                  | SYMBOL of string
                  | BOOL of bool
                  | INT of int
                  | STRING of string
                  | LIST of mal_type list
                  | ATOM of mal_type ref
                  | FN of mal_type list -> mal_type
                  | FN4 of mal_env -> mal_type list -> mal_type
                  | FN6 of mal_env -> mal_type list -> (mal_env * mal_type)

and mal_env = ENV of (string * mal_type) list

fun truthy (BOOL false) = false
  | truthy NIL          = false
  | truthy _            = true

fun malEq (     NIL, NIL)      = true
  | malEq (SYMBOL a, SYMBOL b) = a = b
  | malEq (  BOOL a, BOOL b)   = a = b
  | malEq (   INT a, INT b)    = a = b
  | malEq (STRING a, STRING b) = a = b
  | malEq (  LIST a, LIST b)   = ListPair.allEq malEq (a, b)
  | malEq _                    = false
