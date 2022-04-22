datatype mal_type = NIL
                  | SYMBOL of string
                  | BOOL of bool
                  | INT of LargeInt.int
                  | STRING of string
                  | KEYWORD of string
                  | LIST of (mal_type list * mal_meta)
                  | VECTOR of (mal_type list * mal_meta)
                  | MAP of ((mal_type * mal_type) list * mal_meta)
                  | ATOM of mal_type ref
                  | FN of (mal_type list -> mal_type) * mal_meta
                  | MACRO of mal_type list -> mal_type

and mal_meta = META of mal_type
             | NO_META

and mal_ns = NS of (string * mal_type) list ref

and mal_env = ENV of mal_ns
            | INNER of mal_ns * mal_env

fun truthy (BOOL false) = false
  | truthy NIL          = false
  | truthy _            = true

fun malEq (         NIL, NIL)          = true
  | malEq (    SYMBOL a, SYMBOL b)     = a = b
  | malEq (      BOOL a, BOOL b)       = a = b
  | malEq (       INT a, INT b)        = a = b
  | malEq (    STRING a, STRING b)     = a = b
  | malEq (   KEYWORD a, KEYWORD b)    = a = b
  | malEq (  LIST (a,_), LIST (b,_))   = ListPair.allEq malEq (a, b)
  | malEq (VECTOR (a,_), VECTOR (b,_)) = ListPair.allEq malEq (a, b)
  | malEq (  LIST (a,_), VECTOR (b,_)) = ListPair.allEq malEq (a, b)
  | malEq (VECTOR (a,_), LIST (b,_))   = ListPair.allEq malEq (a, b)
  | malEq (   MAP (a,_), MAP (b,_))    = mapEq a b
  | malEq _                            = false
and mapEq a b =
    a |> List.map (fn (k,va) => (va, malGet b k)) |> List.all (fn (va,SOME vb) => malEq (va, vb) | _ => false) andalso
    b |> List.map (fn (k,vb) => (vb, malGet a k)) |> List.all (fn (vb,SOME va) => malEq (vb, va) | _ => false)

and malGet m k = m |> List.find (fn (k',_) => malEq (k, k')) |> Option.map #2
and malAssoc m k v = (k, v) :: (malDissoc m k)
and malDissoc m k = m |> List.filter (not o (fn (k', _) => malEq (k, k')))

fun malList xs = LIST (xs, NO_META)
fun malVector xs = VECTOR (xs, NO_META)
fun malMap kvps = MAP (kvps, NO_META)
