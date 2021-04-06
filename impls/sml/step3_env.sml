exception NotDefined of string
exception NotApplicable of string

fun read s =
    readStr s

fun eval e (LIST (a::args,_)) = (case specialEval a of SOME special => special e args | _ => evalApply e (eval e a) args)
  | eval e (SYMBOL s)         = evalSymbol e s
  | eval e (VECTOR (v,_))     = VECTOR (map (eval e) v, NO_META)
  | eval e (MAP (m,_))        = MAP (List.map (fn (k, v) => (eval e k, eval e v)) m, NO_META)
  | eval e ast                = ast

and specialEval (SYMBOL "def!") = SOME evalDef
  | specialEval (SYMBOL "let*") = SOME evalLet
  | specialEval _               = NONE

and evalDef e [SYMBOL s, ast] = let val v = eval e ast in (def s v e; v) end
  | evalDef _ _ = raise NotApplicable "def! needs a symbol and a form to evaluate"

and evalLet e [LIST (bs,_), ast]   = eval (bind bs (inside e)) ast
  | evalLet e [VECTOR (bs,_), ast] = eval (bind bs (inside e)) ast
  | evalLet _ _ = raise NotApplicable "let* needs a list of bindings and a form to evaluate"

and evalApply e (FN (f,_)) args = f (map (eval e) args)
  | evalApply _ a args = raise NotApplicable (prStr a ^ " is not applicable on " ^ prStr (LIST (args, NO_META)))

and evalSymbol e s = valOrElse (lookup e s)
                               (fn _ => raise NotDefined ("symbol '" ^ s ^ "' not found"))

and bind (SYMBOL s::v::rest) e = (def s (eval e v) e; bind rest e)
  | bind []                  e = e
  | bind _ _ = raise NotApplicable "bindings must be a list of symbol/form pairs"

fun print f =
    prReadableStr f

fun rep e s =
    s |> read |> eval e |> print
    handle Nothing           => ""
         | SyntaxError msg   => "SYNTAX ERROR: " ^ msg
         | NotApplicable msg => "CANNOT APPLY: " ^ msg
         | NotDefined msg    => "NOT DEFINED: "  ^ msg

fun malPlus  (INT a, INT b) = INT (a + b)
  | malPlus _ = raise NotApplicable "can only add integers"
fun malTimes (INT a, INT b) = INT (a * b)
  | malTimes _ = raise NotApplicable "can only multiply integers"
fun malMinus (INT b, INT a) = INT (a - b)
  | malMinus _ = raise NotApplicable "can only subtract integers"
fun malDiv   (INT b, INT a) = INT (a div b)
  | malDiv _ = raise NotApplicable "can only divide integers"

val replEnv = ENV (NS (ref [])) |> bind [
    SYMBOL "+",
    FN (foldl malPlus (INT 0), NO_META),
    SYMBOL "*",
    FN (foldl malTimes (INT 1), NO_META),
    SYMBOL "-",
    FN (fn [x]   => malMinus (x, INT 0)
         | x::xs => foldr malMinus x xs
         | _ => raise NotApplicable "'-' requires arguments"
        , NO_META),
    SYMBOL "/",
    FN (fn [x]   => malDiv (x, INT 1)
         | x::xs => foldr malDiv x xs
         | _ => raise NotApplicable "'/' requires arguments"
        , NO_META)
]

fun repl e =
    let open TextIO
    in (
        print("user> ");
        case inputLine(stdIn) of
            SOME(line) =>
                let val s = rep e line
                    val _ = print(s ^ "\n")
                in
                    repl e
                end
            | NONE => ()
    ) end

fun main () = repl replEnv
