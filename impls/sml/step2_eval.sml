exception NotDefined of string
exception NotApplicable of string

fun read s =
    readStr s

fun eval e ast = case ast of
    LIST (_::_,_) => evalApply e ast
    | _           => evalAst e ast

and evalAst e ast = case ast of
    SYMBOL s       => (case lookup e s of SOME v => v | NONE => raise NotDefined ("unable to resolve symbol '" ^ s ^ "'"))
    | LIST (l,_)   => LIST (List.map (eval e) l, NO_META)
    | VECTOR (v,_) => VECTOR (List.map (eval e) v, NO_META)
    | MAP (m,_)    => MAP (List.map (fn (k, v) => (eval e k, eval e v)) m, NO_META)
    | _            => ast

and evalApply e ast = case evalAst e ast of
    LIST ((FN (f,_))::args, _) => f args
    | _ => raise NotApplicable "eval_apply needs a non-empty list"

fun print f =
    prReadableStr f

fun rep e s =
    s |> read |> eval e |> print
    handle Nothing => ""
         | e       => "ERROR: " ^ (exnMessage e)

fun malPlus  (INT a, INT b) = INT (a + b)
  | malPlus _ = raise NotApplicable "can only add integers"
fun malTimes (INT a, INT b) = INT (a * b)
  | malTimes _ = raise NotApplicable "can only multiply integers"
fun malMinus (INT b, INT a) = INT (a - b)
  | malMinus _ = raise NotApplicable "can only subtract integers"
fun malDiv   (INT b, INT a) = INT (a div b)
  | malDiv _ = raise NotApplicable "can only divide integers"

val replEnv = ENV (NS (ref [
    ("+", FN (foldl malPlus (INT 0), NO_META)),
    ("*", FN (foldl malTimes (INT 1), NO_META)),
    ("-", FN (
        fn [x]   => malMinus (x, INT 0)
         | x::xs => foldr malMinus x xs
         | _ => raise NotApplicable "'-' requires at least one argument"
        , NO_META
    )),
    ("/", FN (
        fn [x]   => malDiv (x, INT 1)
         | x::xs => foldr malDiv x xs
         | _ => raise NotApplicable "'/' requires at least one argument"
        , NO_META
    ))
]))

fun repl () =
    let open TextIO
    in (
        print("user> ");
        case inputLine(stdIn) of
            SOME(line) => (
                print((rep replEnv line) ^ "\n");
                repl ()
            )
            | NONE => ()
    ) end

fun main () = repl ()
