fun read s =
    readStr s

fun eval e (LIST (a::args)) = (case specialEval a of SOME special => special e args | _ => evalApply e (eval e a) args)
  | eval e (SYMBOL s)       = evalSymbol e s
  | eval e (VECTOR v)       = VECTOR (map (eval e) v)
  | eval e (MAP m)          = MAP (List.map (fn (k, v) => (eval e k, eval e v)) m)
  | eval e ast              = ast

and specialEval (SYMBOL "def!") = SOME evalDef
  | specialEval (SYMBOL "let*") = SOME evalLet
  | specialEval (SYMBOL "do")   = SOME evalDo
  | specialEval (SYMBOL "if")   = SOME evalIf
  | specialEval (SYMBOL "fn*")  = SOME evalFn
  | specialEval _               = NONE

and evalDef e [SYMBOL s, ast] = let val v = eval e ast in (def s v e; v) end
  | evalDef _ _               = raise NotApplicable "def! needs a symbol and a form to evaluate"

and evalLet e [LIST bs, ast]   = eval (bind bs (inside e)) ast
  | evalLet e [VECTOR bs, ast] = eval (bind bs (inside e)) ast
  | evalLet _ _                = raise NotApplicable "let* needs a list of bindings and a form to evaluate"

and evalDo e (x::xs) = foldl (fn (x, _) => eval e x) (eval e x) xs
  | evalDo _ _       = raise NotApplicable "do needs at least one argument"

and evalIf e [c,a,b] = if truthy (eval e c) then eval e a else eval e b
  | evalIf e [c,a]   = evalIf e [c,a,NIL]
  | evalIf _ _       = raise NotApplicable "if needs two or three arguments"

and evalFn e [(LIST binds),body]   = makeFn e binds body
  | evalFn e [(VECTOR binds),body] = makeFn e binds body
  | evalFn _ _                     = raise NotApplicable "fn* needs a list of bindings and a body"
and makeFn e binds body = FN (fn (exprs) => eval (bind (interleave binds exprs) (inside e)) body)

and evalApply e (FN f) args = f (map (eval e) args)
  | evalApply _ x      args = raise NotApplicable (prStr x ^ " is not applicable on " ^ prStr (LIST args))

and evalSymbol e s = valOrElse (lookup e s)
                               (fn _ => raise NotDefined ("symbol '" ^ s ^ "' not found"))

and bind (SYMBOL "&"::v::(SYMBOL s)::vs) e = (def s (LIST (map (eval e) (v::vs))) e; e)
  | bind [SYMBOL "&", SYMBOL s]          e = (def s (LIST []) e; e)
  | bind (SYMBOL s::v::rest)             e = (def s (eval e v) e; bind rest e)
  | bind []                              e = e
  | bind _ _ = raise NotApplicable "bindings must be a list of symbol/form pairs"

fun print f =
    prReadableStr f

fun rep e s =
    s |> read |> eval e |> print
    handle Nothing           => ""
         | SyntaxError msg   => "SYNTAX ERROR: " ^ msg
         | NotApplicable msg => "CANNOT APPLY: " ^ msg
         | NotDefined msg    => "NOT DEFINED: " ^ msg

val initEnv = ENV (NS (ref [])) |> bind coreNs

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

val prelude = "                        \
\(def! not (fn* (a) (if a false true)))"

fun main () = (
    rep initEnv ("(do " ^ prelude ^ " nil)");
    repl initEnv
)
