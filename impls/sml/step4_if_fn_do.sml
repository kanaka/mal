fun read s =
    readStr s

(* TODO def! evaluated inside other forms *)
fun eval e (LIST (SYMBOL "def!"::args)) = evalDef e args
  | eval e (LIST (SYMBOL "let*"::args)) = (e, evalLet e args)
  | eval e (LIST (SYMBOL "do"::args))   = (e, evalDo e args)
  | eval e (LIST (SYMBOL "if"::args))   = (e, evalIf e args)
  | eval e (LIST (SYMBOL "fn*"::args))  = (e, evalFn e args)
  | eval e (LIST (a::args))             = (e, evalApply e (eval' e a) args)
  | eval e (SYMBOL s)                   = (e, evalSymbol e s)
  | eval e ast                          = (e, ast)

and eval' e ast = (#2 o eval e) ast

and evalDef e [SYMBOL s, ast] = let val v = eval' e ast in (def s v e, v) end
  | evalDef _ _               = raise NotApplicable "def! needs a symbol and a form to evaluate"

and evalLet e [LIST bs, ast] = eval' (bind bs e) ast
  | evalLet _ _              = raise NotApplicable "let* needs a list of bindings and a form to evaluate"

and evalDo e (args as _::_) = map (eval' e) args |> List.last
  | evalDo _ _              = raise NotApplicable "do needs at least one argument"

and evalIf e [c,a,b] = if truthy (eval' e c) then (eval' e a) else (eval' e b)
  | evalIf e [c,a]   = if truthy (eval' e c) then (eval' e a) else NIL
  | evalIf _ _       = raise NotApplicable "if needs two or three arguments"

and evalFn e [(LIST binds),body] = FN (fn (exprs) => eval' (bind (interleave binds exprs) e) body)
  | evalFn _ _                   = raise NotApplicable "fn* needs a list of bindings and a body"

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

val initEnv = ENV [] |> bind coreNs

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
