exception NotDefined of string
exception NotApplicable of string

fun READ s =
    readStr s

fun EVAL e (LIST (SYMBOL "def!"::args)) = eval_def e args
  | EVAL e (ast as (LIST (_::_)))       = (e, eval_apply e ast)
  | EVAL e ast                          = (e, eval_ast e ast)

and eval_ast e (LIST l)   = LIST (List.map (#2 o (EVAL e)) l)
  | eval_ast e (SYMBOL s) = valOrElse (lookup e s) (fn _ => raise NotDefined ("symbol '" ^ s ^ "' not found"))
  | eval_ast e ast        = ast

and eval_apply e ast =
    case eval_ast e ast of
        LIST ((FN f)::args) => f args
        | _ => raise NotApplicable "eval_apply needs a non-empty list"

and eval_def e [SYMBOL s, ast] = let val (_, v) = EVAL e ast in (def s v e, v) end
  | eval_def _ _ = raise NotApplicable "define needs a symbol and a value"

fun PRINT f =
    prStr f

fun rep e s =
    s |> READ |> EVAL e |> (fn (e, v) => (e, PRINT v))
    handle Nothing           => (e, "")
         | SyntaxError msg   => (e, "SYNTAX ERROR: " ^ msg)
         | NotApplicable msg => (e, "CANNOT APPLY: " ^ msg)
         | NotDefined msg    => (e, "NOT DEFINED: " ^ msg)

fun malPlus  (INT a, INT b) = INT (a + b)
  | malPlus _ = raise NotApplicable "can only add integers"
fun malTimes (INT a, INT b) = INT (a * b)
  | malTimes _ = raise NotApplicable "can only multiply integers"
fun malMinus (INT b, INT a) = INT (a - b)
  | malMinus _ = raise NotApplicable "can only subtract integers"
fun malDiv   (INT b, INT a) = INT (a div b)
  | malDiv _ = raise NotApplicable "can only divide integers"

val initEnv =
  [] |> set "+"
        (FN (foldl malPlus (INT 0)))
     |> set "*"
        (FN (foldl malTimes (INT 1)))
     |> set "-"
        (FN (fn [x]   => malMinus (x, INT 0)
              | x::xs => foldr malMinus x xs
              | _     => raise NotApplicable "'-' requires arguments"))
     |> set "/"
        (FN (fn [x]   => malDiv (x, INT 1)
              | x::xs => foldr malDiv x xs
              | _     => raise NotApplicable "'/' requires arguments"))
     |> ENV

fun repl () = repl' initEnv

and repl' e =
    let open TextIO
    in (
        print("user> ");
        case inputLine(stdIn) of
            SOME(line) =>
                let val (e', s) = rep e line
                    val _ = print(s ^ "\n")
                in
                    repl' e'
                end
            | NONE => ()
    ) end
