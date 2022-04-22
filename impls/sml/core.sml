exception NotDefined of string
exception NotApplicable of string
exception OutOfBounds of string
exception MalException of mal_type

(*
 * Some helper functions
 *)

fun buildMap (k::v::rest) acc = buildMap rest (malAssoc acc k v) 
  | buildMap []           acc = malMap (rev acc)
  | buildMap _ _ = raise NotApplicable "maps can only be constructed from an even number of arguments"

fun collectLists ls = collectLists' ls []
and collectLists' (LIST (l,_)::rest)   acc = collectLists' rest (l::acc)
  | collectLists' (VECTOR (v,_)::rest) acc = collectLists' rest (v::acc)
  | collectLists' []                   acc = rev acc
  | collectLists' _                    _   = raise NotApplicable "invalid arguments"

fun arithFolder n f (INT next, INT prev) = INT (f (prev, next))
  | arithFolder n _ _ = raise NotApplicable ("'" ^ n ^ "' requires integer arguments")

fun cmpFolder n c (INT next, (INT prev, acc)) = (INT next, acc andalso (c (prev, next)))
  | cmpFolder n _ _ = raise NotApplicable ("'" ^ n ^ "' requires integer arguments")

fun cmpFold n c (x::xs) = foldl (cmpFolder n c) (x, true) xs |> #2 |> BOOL
  | cmpFold n _ _ = raise NotApplicable ("'" ^ n ^ "' requires arguments")

fun splatArgs [LIST (l,_)]   = l
  | splatArgs [VECTOR (v,_)] = v
  | splatArgs (x::xs)        = x::(splatArgs xs)
  | splatArgs []             = []

fun slurp lines strm = case TextIO.inputLine strm of
    SOME l => slurp (l::lines) strm
    | NONE => (TextIO.closeIn strm; rev lines)

fun malPrint s = (
    TextIO.print (s ^ "\n");
    NIL
)

fun readLine prompt = (
    TextIO.print prompt;
    TextIO.inputLine TextIO.stdIn |> Option.map (trimr 1)
)

fun strJoin separator strings = String.concatWith separator strings

(*
 * Core primitives
 *)

fun prim name f =
    let val badArgs = STRING ("incorrect arguments passed to '" ^ name ^ "'") in
        [SYMBOL name, FN (fn args => f args handle Domain => raise MalException badArgs, NO_META)]
    end

val coreNs = List.concat [

    (* Maths *)
    prim "+" (fn args    => foldl (arithFolder "+" (op +)) (INT 0) args),
    prim "*" (fn args    => foldl (arithFolder "*" (op * )) (INT 1) args),
    prim "/" (fn (x::xs) => foldl (arithFolder "/" (op div)) x xs | _ => raise Domain),
    prim "-" (fn (x::xs) => foldl (arithFolder "-" (op -)) x xs | _ => raise Domain),
    
    (* Comparisons *)
    prim "<"  (cmpFold "<"  (op <)),
    prim "<=" (cmpFold "<=" (op <=)),
    prim ">=" (cmpFold ">=" (op >=)),
    prim ">"  (cmpFold ">"  (op >)),
    prim "="
    (fn (x::xs) => foldl (fn (n,(p,acc)) => (n,acc andalso (malEq (n, p)))) (x, true) xs |> #2 |> BOOL
      | _ => raise Domain),

    (* Predicates *)
    prim     "nil?" (fn [NIL]        => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim    "true?" (fn [BOOL true]  => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim   "false?" (fn [BOOL false] => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim  "symbol?" (fn [SYMBOL _]   => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim "keyword?" (fn [KEYWORD _]  => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim  "vector?" (fn [VECTOR _]   => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim     "map?" (fn [MAP _]      => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim      "fn?" (fn [FN _]       => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim   "macro?" (fn [MACRO _]    => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim  "string?" (fn [STRING _]   => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim  "number?" (fn [INT _]      => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim    "atom?" (fn [ATOM _]     => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim    "list?" (fn [LIST _]     => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim "sequential?"
    (fn [LIST _] => BOOL true | [VECTOR _] => BOOL true | [_] => BOOL false | _ => raise Domain),
    prim "empty?"
    (fn [LIST (l,_)] => BOOL (length l = 0) | [VECTOR (v,_)] => BOOL (length v = 0) | _ => raise Domain),
    prim "contains?"
    (fn [MAP (m,_), k] => BOOL (List.exists (fn (k', _) => malEq (k, k')) m) | _ => raise Domain),

    (* I/O *)
    prim "slurp"
    (fn [STRING filename] => TextIO.openIn filename |> slurp [] |> strJoin "" |> STRING | _ => raise Domain),
    prim "prn"
    (fn args => args |> map prReadableStr |> strJoin " " |> malPrint),
    prim "println"
    (fn args => args |> map prStr |> strJoin " " |> malPrint),
    prim "readline"
    (fn [STRING prompt] => valOrElse (readLine prompt |> Option.map STRING) (fn () => NIL) | _ => raise Domain),

    (* Strings and stringoids *)
    prim "str"
    (fn args => args |> map prStr |> strJoin "" |> STRING),
    prim "pr-str"
    (fn args => args |> map prReadableStr |> strJoin " " |> STRING),
    prim "symbol"
    (fn [STRING s] => SYMBOL s | _ => raise Domain),
    prim "keyword"
    (fn [STRING s] => KEYWORD s | [kw as KEYWORD _] => kw | _ => raise Domain),

    (* Atoms *)
    prim "atom"   (fn [x] => ATOM (ref x) | _ => raise Domain),
    prim "deref"  (fn [ATOM a] => !a | _ => raise Domain),
    prim "reset!" (fn [ATOM a, x] => (a := x; x) | _ => raise Domain),
    prim "swap!"  (fn (ATOM a::(FN (f,_))::args) => let val x = f ((!a)::args) in (a := x; x) end | _ => raise Domain),

    (* Listoids *)
    prim "list"   (fn args => malList args),
    prim "vector" (fn args => malVector (args)),
    prim "vec"    (fn [LIST (xs,_)] => malVector (xs) | [v as VECTOR _] => v | _ => raise Domain),
    prim "concat" (fn args => malList (List.concat (collectLists args))),
    prim "cons"
    (fn [hd, LIST (tl,_)]   => malList (hd::tl)
      | [hd, VECTOR (tl,_)] => malList (hd::tl)
      | _ => raise Domain),
    prim "conj"
    (fn (LIST (l,_)::args)   => malList (rev args @ l)
      | (VECTOR (v,_)::args) => malVector (v @ args)
      | _ => raise Domain),
    prim "seq"
    (fn [LIST ([],_)]   => NIL | [l as LIST _]  => l
      | [VECTOR ([],_)] => NIL | [VECTOR (v,_)] => malList v
      | [STRING ""]     => NIL | [STRING s]     => String.explode s |> List.map (STRING o String.str) |> malList
      | [NIL]           => NIL
      | _ => raise Domain),
    prim "count"
    (fn [LIST (l,_)]   => INT (length l |> LargeInt.fromInt)
      | [VECTOR (v,_)] => INT (length v |> LargeInt.fromInt)
      | [NIL]          => INT 0
      | _ => raise Domain),
    prim "nth"
    (fn [LIST (l,_), INT n]   => (List.nth (l, (Int.fromLarge n)) handle Subscript => raise OutOfBounds "index out of bounds")
      | [VECTOR (v,_), INT n] => (List.nth (v, (Int.fromLarge n)) handle Subscript => raise OutOfBounds "index out of bounds")
      | _ => raise Domain),
    prim "first"
    (fn [LIST (l,_)]   => (case l of (x::_) => x | _ => NIL)
      | [VECTOR (v,_)] => (case v of (x::_) => x | _ => NIL)
      | [NIL]          => NIL
      | _ => raise Domain),
    prim "rest"
    (fn [LIST (l,_)]   => malList (case l of (_::xs) => xs | _ => [])
      | [VECTOR (v,_)] => malList (case v of (_::xs) => xs | _ => [])
      | [NIL]          => malList ([])
      | _ => raise Domain),
    prim "map"
    (fn [FN (f,_), LIST (l,_)]   => malList (List.map (fn x => f [x]) l)
      | [FN (f,_), VECTOR (v,_)] => malList (List.map (fn x => f [x]) v)
      | _ => raise Domain),

    (* Maps *)
    prim "hash-map"
    (fn args => buildMap args []),
    prim "assoc"
    (fn (MAP (m,_)::(args as _::_)) => buildMap args m | _ => raise Domain),
    prim "dissoc"
    (fn (MAP (m,_)::(args as _::_)) => malMap (foldl (fn (k, acc) => malDissoc acc k) m args) | _ => raise Domain),
    prim "get"
    (fn [MAP (m,_), k] => valOrElse (malGet m k) (fn () => NIL) | [NIL, _] => NIL | _ => raise Domain),
    prim "keys"
    (fn [MAP (m,_)] => malList (map #1 m) | _ => raise Domain),
    prim "vals"
    (fn [MAP (m,_)] => malList (map #2 m) | _ => raise Domain),

    (* Metaprogramming and metadata *)
    prim "read-string"
    (fn [STRING s] => readStr s | _ => raise Domain),
    prim "apply"
    (fn (FN (f,_)::args) => f (splatArgs args) | _ => raise Domain),
    prim "meta"
    (fn [    FN (_, META m)] => m
      | [  LIST (_, META m)] => m
      | [VECTOR (_, META m)] => m
      | [   MAP (_, META m)] => m
      | [_] => NIL
      | _ => raise Domain),
    prim "with-meta"
    (fn [FN (f,_),     meta] =>     FN (f, META meta)
      | [LIST (l,_),   meta] =>   LIST (l, META meta)
      | [VECTOR (v,_), meta] => VECTOR (v, META meta)
      | [MAP (m,_),    meta] =>    MAP (m, META meta)
      | [x] => x
      | _ => raise Domain),

    (* Odds and ends *)
    prim "throw"
    (fn [x] => raise MalException x | _ => raise Domain),
    prim "time-ms"
    (fn _ => INT (Time.now () |> Time.toMilliseconds))
]
