datatype mal_type = NIL
                  | SYMBOL of string
                  | BOOL of bool
                  | INT of int
                  | STRING of string
                  | LIST of mal_type list
                  | ATOM of mal_type ref
                  | FN of mal_type list -> mal_type

and mal_ns = NS of (string * mal_type) list ref

and mal_env = ENV of mal_ns
            | INNER of mal_ns * mal_env

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
