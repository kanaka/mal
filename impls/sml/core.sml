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

val coreIo = [
    SYMBOL "prn",
    FN (fn [x] => (TextIO.print ((prStr x) ^ "\n"); NIL)
         | _   => raise NotApplicable "'prn' requires one argument")
]

fun intFun n f r [INT a, INT b] = r (f (a, b))
  | intFun n _ _ _              = raise NotApplicable ("'" ^ n ^ "' requires two integer arguments")

(* TODO: variadic versions? *)
val coreCmp = [
    SYMBOL "=",
    FN (fn [a, b] => BOOL (malEq (a, b))
         | _      => raise NotApplicable "'=' requires two arguments"),

    SYMBOL "<",  FN (intFun "<"  (op <)  BOOL),
    SYMBOL "<=", FN (intFun "<=" (op <=) BOOL),
    SYMBOL ">=", FN (intFun ">=" (op >=) BOOL),
    SYMBOL ">",  FN (intFun ">"  (op >)  BOOL)
]

(* TODO: variadic versions? *)
val coreMath = [
    SYMBOL "+", FN (intFun "+" (op +)   INT),
    SYMBOL "*", FN (intFun "*" (op * )  INT), (* mosml can't parse*)
    SYMBOL "-", FN (intFun "-" (op -)   INT),
    SYMBOL "/", FN (intFun "/" (op div) INT)
]

val coreNs = List.concat [
    coreList,
    coreIo,
    coreCmp,
    coreMath
]
