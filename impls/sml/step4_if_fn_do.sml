exception NotDefined of string
exception NotApplicable of string

fun read s =
    readStr s

fun eval e (LIST (SYMBOL "def!"::args)) = evalDef e args
  | eval e (LIST (SYMBOL "let*"::args)) = evalLet e args
  | eval e (LIST (a::args))             = (e, evalApply e (eval' e a) args)
  | eval e (SYMBOL s)                   = (e, evalSymbol e s)
  | eval e ast                          = (e, ast)

and eval' e ast = (#2 o eval e) ast

and evalDef e [SYMBOL s, ast] = let val v = eval' e ast in (def s v e, v) end
  | evalDef _ _               = raise NotApplicable "def! needs a symbol and a form to evaluate"

and evalLet e [LIST bs, ast] = (e, eval' (bind bs e) ast)
  | evalLet _ _              = raise NotApplicable "let* needs a list of bindings and a form to evaluate"

and evalApply e (FN f) args = f (map (eval' e) args)
  | evalApply _ a      args = raise NotApplicable (prStr a ^ " is not applicable on " ^ prStr (LIST args))

and evalSymbol e s = valOrElse (lookup e s)
                               (fn _ => raise NotDefined ("symbol '" ^ s ^ "' not found"))

and bind (SYMBOL s::v::rest) e = def s (eval' e v) e |> bind rest
  | bind []                  e = e
  | bind _ _ = raise NotApplicable "bindings must be a list of symbol/form pairs"

fun print f =
    prStr f

fun rep e s =
    s |> read |> eval e |> (fn (e, v) => (e, print v))
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

val initEnv = ENV [] |> bind [
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
