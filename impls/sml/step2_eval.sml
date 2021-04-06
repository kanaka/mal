exception NotDefined of string
exception NotApplicable of string

fun READ s =
    readStr s

fun EVAL e ast = case ast of
    LIST (_::_,_) => eval_apply e ast
    | _           => eval_ast e ast

and eval_ast e ast = case ast of
    SYMBOL s       => (case lookup e s of SOME v => v | NONE => raise NotDefined ("unable to resolve symbol '" ^ s ^ "'"))
    | LIST (l,_)   => LIST (List.map (EVAL e) l, NO_META)
    | VECTOR (v,_) => VECTOR (List.map (EVAL e) v, NO_META)
    | MAP (m,_)    => MAP (List.map (fn (k, v) => (EVAL e k, EVAL e v)) m, NO_META)
    | _            => ast

and eval_apply e ast = case eval_ast e ast of
    LIST ((FN (f,_))::args, _) => f args
    | _ => raise NotApplicable "eval_apply needs a non-empty list"

fun PRINT f =
    prReadableStr f

fun rep e s =
    s |> READ |> EVAL e |> PRINT
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
