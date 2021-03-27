exception NotDefined of string
exception NotApplicable of string

fun malPlus  (INT a, INT b) = INT (a + b)
  | malPlus _ = raise NotApplicable "can only add integers"
fun malTimes (INT a, INT b) = INT (a * b)
  | malTimes _ = raise NotApplicable "can only multiply integers"
fun malMinus (INT b, INT a) = INT (a - b)
  | malMinus _ = raise NotApplicable "can only subtract integers"
fun malDiv   (INT b, INT a) = INT (a div b)
  | malDiv _ = raise NotApplicable "can only divide integers"

val coreMath = [
    SYMBOL "+",
    FN (foldl malPlus (INT 0)),

    SYMBOL "*",
    FN (foldl malTimes (INT 1)),

    SYMBOL "-",
    FN (fn [x]   => malMinus (x, INT 0)
         | x::xs => foldr malMinus x xs
         | _     => raise NotApplicable "'-' requires arguments"),

    SYMBOL "/",
    FN (fn [x]   => malDiv (x, INT 1)
         | x::xs => foldr malDiv x xs
         | _     => raise NotApplicable "'/' requires arguments")
]

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
         | _        => raise NotApplicable "count requires a list")
]


val coreNs = List.concat [
    coreList,
    coreMath
]
