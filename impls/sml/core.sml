exception NotDefined of string
exception NotApplicable of string

val coreList = [
    SYMBOL "list",
    FN (fn args => LIST args),

    SYMBOL "list?",
    FN (fn [LIST _] => BOOL true
         | [_]      => BOOL false
         | _        => raise NotApplicable "list? requires one argument"),

    SYMBOL "empty?",
    FN (fn [LIST []] => BOOL true
         | [LIST _]  => BOOL false
         | _         => raise NotApplicable "empty? requires a list"),

    SYMBOL "count",
    FN (fn [LIST l] => INT (length l)
         | [NIL]    => INT 0
         | _        => raise NotApplicable "count requires a list")
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
    FN (fn args => args |> List.map prReadableStr |> String.concatWith " " |> malPrint),

    SYMBOL "println",
    FN (fn args => args |> List.map prStr         |> String.concatWith " " |> malPrint)
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
    SYMBOL ">",  FN (cmpFold ">" (op >))
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
    FN (fn args => args |> List.map prReadableStr |> String.concatWith " " |> STRING),

    SYMBOL "str",
    FN (fn args => args |> List.map prStr         |> String.concatWith ""  |> STRING)
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

val coreNs = List.concat [
    coreList,
    coreIo,
    coreCmp,
    coreMeta,
    coreString,
    coreAtom,
    coreMath
]
