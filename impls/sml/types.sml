datatype mal_type = NIL
                  | SYMBOL of string
                  | BOOL of bool
                  | INT of int
                  | STRING of string
                  | KEYWORD of string
                  | LIST of mal_type list
                  | VECTOR of mal_type list
                  | MAP of (mal_type * mal_type) list
                  | ATOM of mal_type ref
                  | FN of mal_type list -> mal_type
                  | MACRO of mal_type list -> mal_type

and mal_ns = NS of (string * mal_type) list ref

and mal_env = ENV of mal_ns
            | INNER of mal_ns * mal_env

fun truthy (BOOL false) = false
  | truthy NIL          = false
  | truthy _            = true

fun malEq (      NIL, NIL)       = true
  | malEq ( SYMBOL a, SYMBOL b)  = a = b
  | malEq (   BOOL a, BOOL b)    = a = b
  | malEq (    INT a, INT b)     = a = b
  | malEq ( STRING a, STRING b)  = a = b
  | malEq (KEYWORD a, KEYWORD b) = a = b
  | malEq (   LIST a, LIST b)    = ListPair.allEq malEq (a, b)
  | malEq ( VECTOR a, VECTOR b)  = ListPair.allEq malEq (a, b)
  | malEq (   LIST a, VECTOR b)  = ListPair.allEq malEq (a, b)
  | malEq ( VECTOR a, LIST b)    = ListPair.allEq malEq (a, b)
  | malEq (    MAP a, MAP b)     = mapEq a b
  | malEq _                      = false
and mapEq a b =
    a |> List.map (fn (k,va) => (va, malGet b k)) |> List.all (fn (va,SOME vb) => malEq (va, vb) | _ => false) andalso
    b |> List.map (fn (k,vb) => (vb, malGet a k)) |> List.all (fn (vb,SOME va) => malEq (vb, va) | _ => false)

and malGet m k = m |> List.find (fn (k',_) => malEq (k, k')) |> Option.map #2

and malAssoc m k v = (k, v) :: (malDissoc m k)

and malDissoc m k = m |> List.filter (not o (fn (k', _) => malEq (k, k')))
