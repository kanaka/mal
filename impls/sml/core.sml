exception NotDefined of string
exception NotApplicable of string
exception OutOfBounds of string
exception MalException of mal_type

fun collectLists ls = collectLists' ls []
and collectLists' (LIST l::rest)   acc = collectLists' rest (l::acc)
  | collectLists' (VECTOR v::rest) acc = collectLists' rest (v::acc)
  | collectLists' []               acc = SOME (rev acc)
  | collectLists' _                _   = NONE

fun buildMap (k::v::rest) acc = buildMap rest (malAssoc acc k v) 
  | buildMap []           acc = MAP (rev acc)
  | buildMap _ _ = raise NotApplicable "maps can only be constructed from an even number of arguments"

val coreCollection = [
    SYMBOL "list",
    FN (fn args => LIST args),

    SYMBOL "list?",
    FN (fn [LIST _] => BOOL true
         | [_]      => BOOL false
         | _        => raise NotApplicable "list? requires one argument"),

    SYMBOL "empty?",
    FN (fn [LIST []]   => BOOL true
         | [LIST _]    => BOOL false
         | [VECTOR []] => BOOL true
         | [VECTOR _]  => BOOL false
         | _           => raise NotApplicable "empty? requires a list"),

    SYMBOL "count",
    FN (fn [LIST l]   => INT (length l)
         | [VECTOR v] => INT (length v)
         | [NIL]      => INT 0
         | _          => raise NotApplicable "count requires a list"),

    SYMBOL "cons",
    FN (fn [hd,LIST tl]   => LIST (hd::tl)
         | [hd,VECTOR tl] => LIST (hd::tl)
         | _              => raise NotApplicable "cons requires an element and a list or vector"),

    SYMBOL "concat",
    FN (fn args => case collectLists args of
            SOME ls => LIST (List.concat ls)
            | NONE  => raise NotApplicable "concat requires an element and a list or vector"),

    SYMBOL "vec",
    FN (fn [LIST xs]       => VECTOR xs
         | [v as VECTOR _] => v
         | x => raise NotApplicable "vec requires a list or vector"),

    SYMBOL "nth",
    FN (fn [LIST l, INT n]   => (List.nth (l, n) handle Subscript => raise OutOfBounds "index out of bounds")
         | [VECTOR v, INT n] => (List.nth (v, n) handle Subscript => raise OutOfBounds "index out of bounds")
         | x => raise NotApplicable "nth requires a list or vector and an index"),

    SYMBOL "first",
    FN (fn [LIST l]   => (case l of (x::_) => x | _ => NIL)
         | [VECTOR v] => (case v of (x::_) => x | _ => NIL)
         | [NIL]      => NIL
         | x => raise NotApplicable "first requires a list or vector or nil"),

    SYMBOL "rest",
    FN (fn [LIST l]   => (case l of (_::xs) => LIST xs | _ => LIST [])
         | [VECTOR v] => (case v of (_::xs) => LIST xs | _ => LIST [])
         | [NIL]      => LIST []
         | x => raise NotApplicable "rest requires a list or vector or nil"),

    SYMBOL "vector",
    FN (fn args => VECTOR args),

    SYMBOL "hash-map",
    FN (fn args => buildMap args []),

    SYMBOL "assoc",
    FN (fn (MAP m :: (args as _::_)) => buildMap args m
         | _ => raise NotApplicable "assoc requires a map and some arguments to add"),

    SYMBOL "dissoc",
    FN (fn (MAP m :: (args as _::_)) => MAP (foldl (fn (k, acc) => malDissoc acc k) m args)
         | _ => raise NotApplicable "dissoc requires a map and some arguments to remove"),

    SYMBOL "get",
    FN (fn [MAP m, k] => valOrElse (malGet m k) (fn () => NIL)
         | [NIL, _] => NIL
         | _ => raise NotApplicable "get requires a map and a key"),

    SYMBOL "contains?",
    FN (fn [MAP m, k] => BOOL (List.exists (fn (k', _) => malEq (k, k')) m)
         | _ => raise NotApplicable "contains? requires a map and a key"),

    SYMBOL "keys",
    FN (fn [MAP m] => LIST (map #1 m)
         | _ => raise NotApplicable "keys requires a map"),

    SYMBOL "vals",
    FN (fn [MAP m] => LIST (map #2 m)
         | _ => raise NotApplicable "vals requires a map")
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
    FN (fn [STRING filename] => (slurp [] (TextIO.openIn filename) |> String.concat |> STRING handle IO.Io _ => NIL)
         | _ => raise NotApplicable "'slurp' requires a string filename"),

    SYMBOL "prn",
    FN (fn args => args |> map prReadableStr |> String.concatWith " " |> malPrint),

    SYMBOL "println",
    FN (fn args => args |> map prStr         |> String.concatWith " " |> malPrint)
]

fun arithFolder n f (INT next, INT prev) = INT (f (prev, next))
  | arithFolder n _ _                    = raise NotApplicable ("'" ^ n ^ "' requires integer arguments")

fun cmpFolder n c (INT next, (INT prev, acc)) = (INT next, acc andalso (c (prev, next)))
  | cmpFolder n _ _                           = raise NotApplicable ("'" ^ n ^ "' requires integer arguments")

fun cmpFold n c (x::xs) = foldl (cmpFolder n c) (x, true) xs |> #2 |> BOOL
  | cmpFold n _ _       = raise NotApplicable ("'" ^ n ^ "' requires arguments")

fun eqFolder (next, (prev, acc)) = (next, acc andalso (malEq (next, prev)))

val coreCmp = [
    SYMBOL "=", 
    FN (fn (x::xs) => foldl eqFolder (x, true) xs |> #2 |> BOOL
         | _       => raise NotApplicable "'=' requires arguments"),

    SYMBOL "<",  FN (cmpFold "<" (op <)),
    SYMBOL "<=", FN (cmpFold "<=" (op <=)),
    SYMBOL ">=", FN (cmpFold ">=" (op >=)),
    SYMBOL ">",  FN (cmpFold ">" (op >)),

    SYMBOL "nil?",
    FN (fn [NIL] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "nil? requires one argument"),

    SYMBOL "true?",
    FN (fn [BOOL true] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "true? requires one argument"),

    SYMBOL "false?",
    FN (fn [BOOL false] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "false? requires one argument"),

    SYMBOL "symbol?",
    FN (fn [SYMBOL _] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "symbol? requires one argument"),

    SYMBOL "keyword?",
    FN (fn [KEYWORD _] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "keyword? requires one argument"),

    SYMBOL "vector?",
    FN (fn [VECTOR _] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "vector? requires one argument"),

    SYMBOL "sequential?",
    FN (fn [LIST _] => BOOL true | [VECTOR _] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "sequential? requires one argument"),

    SYMBOL "map?",
    FN (fn [MAP _] => BOOL true | [_] => BOOL false
         | _ => raise NotApplicable "map? requires one argument")
]

val coreMath = [
    SYMBOL "+", FN (fn args => foldl (arithFolder "+" (op +)) (INT 0) args),
    SYMBOL "*", FN (fn args => foldl (arithFolder "*" (op * )) (INT 1) args),
    SYMBOL "/", FN (fn (x::xs) => foldl (arithFolder "/" (op div)) x xs
                      | _      => raise NotApplicable "'/' requires arguments"),
    SYMBOL "-", FN (fn (x::xs) => foldl (arithFolder "-" (op -)) x xs
                      | _      => raise NotApplicable "'-' requires arguments")
]

val coreMeta = [
    SYMBOL "read-string",
    FN (fn [STRING s] => readStr s
         | _ => raise NotApplicable "'read-string' requires a string")
]

val coreString = [
    SYMBOL "pr-str",
    FN (fn args => args |> map prReadableStr |> String.concatWith " " |> STRING),

    SYMBOL "str",
    FN (fn args => args |> map prStr         |> String.concatWith ""  |> STRING),

    SYMBOL "symbol",
    FN (fn [STRING s] => SYMBOL s
         | _ => raise NotApplicable "symbol requires a string"),

    SYMBOL "keyword",
    FN (fn [STRING s] => KEYWORD s | [kw as KEYWORD _] => kw
         | _ => raise NotApplicable "keyword requires a string")
]

val coreAtom = [
    SYMBOL "atom",
    FN (fn [x] => ATOM (ref x)
         | _ => raise NotApplicable "'atom' requires one argument"),

    SYMBOL "atom?",
    FN (fn [ATOM _] => BOOL true
         | [_]      => BOOL false
         | _ => raise NotApplicable "'atom?' requires one argument"),

    SYMBOL "deref",
    FN (fn [ATOM a] => !a
         | _ => raise NotApplicable "'deref' requires an atom argument"),

    SYMBOL "reset!",
    FN (fn [ATOM a, x] => (a := x; x)
         | _ => raise NotApplicable "'reset!' requires an atom argument"),

    SYMBOL "swap!",
    FN (fn (ATOM a::(FN f)::args) => let val x = f ((!a)::args) in (a := x; x) end
         | _ => raise NotApplicable "'reset!' requires an atom argument")
]

val coreException = [
     SYMBOL "throw",
     FN (fn [x] => raise MalException x
          | _ => raise NotApplicable "'throw' requires one argument")
]

fun splatArgs [LIST l]   = l
  | splatArgs [VECTOR v] = v
  | splatArgs (x::xs)    = x::(splatArgs xs)
  | splatArgs []         = [] (* this should not happen but I see not harm in being exhaustive here *)

val coreFn = [
     SYMBOL "map",
     FN (fn [FN f, LIST l]   => LIST (List.map (fn x => f [x]) l)
          | [FN f, VECTOR v] => LIST (List.map (fn x => f [x]) v)
          | x => raise NotApplicable "map requires a function and a list or vector"),

     SYMBOL "apply",
     FN (fn (FN f::args) => f (splatArgs args)
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
