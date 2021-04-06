exception NotDefined of string
exception NotApplicable of string
exception OutOfBounds of string
exception MalException of mal_type

fun collectLists ls = collectLists' ls []
and collectLists' (LIST (l,_)::rest)   acc = collectLists' rest (l::acc)
  | collectLists' (VECTOR (v,_)::rest) acc = collectLists' rest (v::acc)
  | collectLists' []                   acc = SOME (rev acc)
  | collectLists' _                    _   = NONE

fun buildMap (k::v::rest) acc = buildMap rest (malAssoc acc k v) 
  | buildMap []           acc = MAP (rev acc, NO_META)
  | buildMap _ _ = raise NotApplicable "maps can only be constructed from an even number of arguments"

fun prim f = FN (f, NO_META)

val coreCollection = [
    SYMBOL "list",
    prim (fn args => LIST (args, NO_META)),

    SYMBOL "list?",
    prim (fn [LIST _] => BOOL true
           | [_]      => BOOL false
           | _ => raise NotApplicable "list? requires one argument"),

    SYMBOL "empty?",
    prim (fn [LIST ([],_)]   => BOOL true
           | [LIST _]        => BOOL false
           | [VECTOR ([],_)] => BOOL true
           | [VECTOR _]      => BOOL false
           | _ => raise NotApplicable "empty? requires a list"),

    SYMBOL "count",
    prim (fn [LIST (l,_)]   => INT (length l)
           | [VECTOR (v,_)] => INT (length v)
           | [NIL]          => INT 0
           | _ => raise NotApplicable "count requires a list"),

    SYMBOL "cons",
    prim (fn [hd, LIST (tl,_)]   => LIST (hd::tl, NO_META)
           | [hd, VECTOR (tl,_)] => LIST (hd::tl, NO_META)
           | _ => raise NotApplicable "cons requires an element and a list or vector"),

    SYMBOL "concat",
    prim (fn args => case collectLists args of
              SOME ls => LIST (List.concat ls, NO_META)
              | _ => raise NotApplicable "concat requires an element and a list or vector"),

    SYMBOL "vec",
    prim (fn [LIST (xs,_)]   => VECTOR (xs, NO_META)
           | [v as VECTOR _] => v
           | x => raise NotApplicable "vec requires a list or vector"),

    SYMBOL "nth",
    prim (fn [LIST (l,_), INT n]   => (List.nth (l, n) handle Subscript => raise OutOfBounds "index out of bounds")
           | [VECTOR (v,_), INT n] => (List.nth (v, n) handle Subscript => raise OutOfBounds "index out of bounds")
           | x => raise NotApplicable "nth requires a list or vector and an index"),

    SYMBOL "first",
    prim (fn [LIST (l,_)]   => (case l of (x::_) => x | _ => NIL)
           | [VECTOR (v,_)] => (case v of (x::_) => x | _ => NIL)
           | [NIL]          => NIL
           | x => raise NotApplicable "first requires a list or vector or nil"),

    SYMBOL "rest",
    prim (fn [LIST (l,_)]   => LIST (case l of (_::xs) => xs | _ => [], NO_META)
           | [VECTOR (v,_)] => LIST (case v of (_::xs) => xs | _ => [], NO_META)
           | [NIL]          => LIST ([], NO_META)
           | x => raise NotApplicable "rest requires a list or vector or nil"),

    SYMBOL "vector",
    prim (fn args => VECTOR (args, NO_META)),

    SYMBOL "hash-map",
    prim (fn args => buildMap args []),

    SYMBOL "assoc",
    prim (fn (MAP (m,_)::(args as _::_)) => buildMap args m
           | _ => raise NotApplicable "assoc requires a map and some arguments to add"),

    SYMBOL "dissoc",
    prim (fn (MAP (m,_)::(args as _::_)) => MAP (foldl (fn (k, acc) => malDissoc acc k) m args, NO_META)
           | _ => raise NotApplicable "dissoc requires a map and some arguments to remove"),

    SYMBOL "get",
    prim (fn [MAP (m,_), k] => valOrElse (malGet m k) (fn () => NIL)
           | [NIL, _] => NIL
           | _ => raise NotApplicable "get requires a map and a key"),

    SYMBOL "contains?",
    prim (fn [MAP (m,_), k] => BOOL (List.exists (fn (k', _) => malEq (k, k')) m)
           | _ => raise NotApplicable "contains? requires a map and a key"),

    SYMBOL "keys",
    prim (fn [MAP (m,_)] => LIST (map #1 m, NO_META)
           | _ => raise NotApplicable "keys requires a map"),

    SYMBOL "vals",
    prim (fn [MAP (m,_)] => LIST (map #2 m, NO_META)
           | _ => raise NotApplicable "vals requires a map"),

    SYMBOL "seq",
    prim (fn _ => raise NotDefined "seq is not yet implemented"),

    SYMBOL "conj",
    prim (fn _ => raise NotDefined "conj is not yet implemented")
]

(* N.B. adds extra newline at end *)
fun slurp lines strm = case TextIO.inputLine strm of
    SOME l => slurp (l::lines) strm
    | NONE => (TextIO.closeIn strm; rev lines)

fun malPrint s = (
    TextIO.print (s ^ "\n");
    NIL
)

val coreIo = [
    SYMBOL "slurp",
    prim (fn [STRING filename] => (slurp [] (TextIO.openIn filename) |> String.concat |> STRING handle IO.Io _ => NIL)
           | _ => raise NotApplicable "'slurp' requires a string filename"),

    SYMBOL "prn",
    prim (fn args => args |> map prReadableStr |> String.concatWith " " |> malPrint),

    SYMBOL "println",
    prim (fn args => args |> map prStr         |> String.concatWith " " |> malPrint),

    SYMBOL "readline",
    prim (fn [STRING prompt] => (TextIO.print prompt;
                                 valOrElse (TextIO.inputLine TextIO.stdIn |> Option.map (STRING o (trimr 1)))
                                           (fn () => NIL))
           | _ => raise NotApplicable "readline requires a string"),

    SYMBOL "time-ms", (* I mean, kind of io? *)
    prim (fn _ => INT (Time.now () |> Time.toMilliseconds |> Int.fromLarge))
]

fun arithFolder n f (INT next, INT prev) = INT (f (prev, next))
  | arithFolder n _ _ = raise NotApplicable ("'" ^ n ^ "' requires integer arguments")

fun cmpFolder n c (INT next, (INT prev, acc)) = (INT next, acc andalso (c (prev, next)))
  | cmpFolder n _ _ = raise NotApplicable ("'" ^ n ^ "' requires integer arguments")

fun cmpFold n c (x::xs) = foldl (cmpFolder n c) (x, true) xs |> #2 |> BOOL
  | cmpFold n _ _ = raise NotApplicable ("'" ^ n ^ "' requires arguments")

fun eqFolder (next, (prev, acc)) = (next, acc andalso (malEq (next, prev)))

val coreCmp = [
    SYMBOL "=", 
    prim (fn (x::xs) => foldl eqFolder (x, true) xs |> #2 |> BOOL
           | _ => raise NotApplicable "'=' requires arguments"),

    SYMBOL "<",  prim (cmpFold "<" (op <)),
    SYMBOL "<=", prim (cmpFold "<=" (op <=)),
    SYMBOL ">=", prim (cmpFold ">=" (op >=)),
    SYMBOL ">",  prim (cmpFold ">" (op >)),

    SYMBOL "nil?",
    prim (fn [NIL] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "nil? requires one argument"),

    SYMBOL "true?",
    prim (fn [BOOL true] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "true? requires one argument"),

    SYMBOL "false?",
    prim (fn [BOOL false] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "false? requires one argument"),

    SYMBOL "symbol?",
    prim (fn [SYMBOL _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "symbol? requires one argument"),

    SYMBOL "keyword?",
    prim (fn [KEYWORD _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "keyword? requires one argument"),

    SYMBOL "vector?",
    prim (fn [VECTOR _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "vector? requires one argument"),

    SYMBOL "sequential?",
    prim (fn [LIST _] => BOOL true | [VECTOR _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "sequential? requires one argument"),

    SYMBOL "map?",
    prim (fn [MAP _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "map? requires one argument"),

    SYMBOL "fn?",
    prim (fn [FN _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "fn? requires one argument"),

    SYMBOL "string?",
    prim (fn [STRING _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "string? requires one argument"),

    SYMBOL "number?",
    prim (fn [INT _] => BOOL true | [_] => BOOL false
           | _ => raise NotApplicable "number? requires one argument")
]

val coreMath = [
    SYMBOL "+", prim (fn args => foldl (arithFolder "+" (op +)) (INT 0) args),
    SYMBOL "*", prim (fn args => foldl (arithFolder "*" (op * )) (INT 1) args),
    SYMBOL "/", prim (fn (x::xs) => foldl (arithFolder "/" (op div)) x xs
                       | _ => raise NotApplicable "'/' requires arguments"),
    SYMBOL "-", prim (fn (x::xs) => foldl (arithFolder "-" (op -)) x xs
                       | _ => raise NotApplicable "'-' requires arguments")
]

val coreMeta = [
    SYMBOL "read-string",
    prim (fn [STRING s] => readStr s
           | _ => raise NotApplicable "read-string requires a string"),

     SYMBOL "meta",
     prim (fn [FN (_, META m)]     => m
            | [LIST (_, META m)]   => m
            | [VECTOR (_, META m)] => m
            | [MAP (_, META m)]    => m
            | [_] => NIL
            | _ => raise NotApplicable "meta requires one argument"),

     SYMBOL "with-meta",
     prim (fn [FN (f,_),     meta] => FN (f, META meta)
            | [LIST (l,_),   meta] => LIST (l, META meta)
            | [VECTOR (v,_), meta] => VECTOR (v, META meta)
            | [MAP (m,_),    meta] => MAP (m, META meta)
            | [x] => x
            | _ => raise NotApplicable "meta requires one argument")
]

val coreString = [
    SYMBOL "pr-str",
    prim (fn args => args |> map prReadableStr |> String.concatWith " " |> STRING),

    SYMBOL "str",
    prim (fn args => args |> map prStr         |> String.concatWith ""  |> STRING),

    SYMBOL "symbol",
    prim (fn [STRING s] => SYMBOL s
           | _ => raise NotApplicable "symbol requires a string"),

    SYMBOL "keyword",
    prim (fn [STRING s] => KEYWORD s | [kw as KEYWORD _] => kw
           | _ => raise NotApplicable "keyword requires a string")
]

val coreAtom = [
    SYMBOL "atom",
    prim (fn [x] => ATOM (ref x)
           | _ => raise NotApplicable "'atom' requires one argument"),

    SYMBOL "atom?",
    prim (fn [ATOM _] => BOOL true
           | [_]      => BOOL false
           | _ => raise NotApplicable "'atom?' requires one argument"),

    SYMBOL "deref",
    prim (fn [ATOM a] => !a
           | _ => raise NotApplicable "'deref' requires an atom argument"),

    SYMBOL "reset!",
    prim (fn [ATOM a, x] => (a := x; x)
           | _ => raise NotApplicable "'reset!' requires an atom argument"),

    SYMBOL "swap!",
    prim (fn (ATOM a::(FN (f,_))::args) => let val x = f ((!a)::args) in (a := x; x) end
           | _ => raise NotApplicable "'reset!' requires an atom argument")
]

val coreException = [
     SYMBOL "throw",
     prim (fn [x] => raise MalException x
            | _ => raise NotApplicable "'throw' requires one argument")
]

fun splatArgs [LIST (l,_)]   = l
  | splatArgs [VECTOR (v,_)] = v
  | splatArgs (x::xs)        = x::(splatArgs xs)
  | splatArgs []             = [] (* this should not happen but I see not harm in being exhaustive here *)

val coreFn = [
     SYMBOL "map",
     prim (fn [FN (f,_), LIST (l,_)]   => LIST (List.map (fn x => f [x]) l, NO_META)
            | [FN (f,_), VECTOR (v,_)] => LIST (List.map (fn x => f [x]) v, NO_META)
            | x => raise NotApplicable "map requires a function and a list or vector"),

     SYMBOL "apply",
     prim (fn (FN (f,_)::args) => f (splatArgs args)
            | x => raise NotApplicable "apply requires a function and a list or vector")
]

val coreNs = List.concat [
    coreCollection,
    coreIo,
    coreCmp,
    coreMeta,
    coreString,
    coreAtom,
    coreException,
    coreFn,
    coreMath
]
