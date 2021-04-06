fun read s =
    readStr s

fun eval e (LIST (a::args,_)) = (case specialEval a of SOME special => special e args | _ => evalApply e (eval e a) args)
  | eval e (SYMBOL s)         = evalSymbol e s
  | eval e (VECTOR (v,_))     = VECTOR (map (eval e) v, NO_META)
  | eval e (MAP (m,_))        = MAP (List.map (fn (k, v) => (eval e k, eval e v)) m, NO_META)
  | eval e ast                = ast

and specialEval (SYMBOL "def!") = SOME evalDef
  | specialEval (SYMBOL "let*") = SOME evalLet
  | specialEval (SYMBOL "do")   = SOME evalDo
  | specialEval (SYMBOL "if")   = SOME evalIf
  | specialEval (SYMBOL "fn*")  = SOME evalFn
  | specialEval _               = NONE

and evalDef e [SYMBOL s, ast] = let val v = eval e ast in (def s v e; v) end
  | evalDef _ _ = raise NotApplicable "def! needs a symbol and a form to evaluate"

and evalLet e [LIST (bs,_), ast]   = eval (bindLet bs (inside e)) ast
  | evalLet e [VECTOR (bs,_), ast] = eval (bindLet bs (inside e)) ast
  | evalLet _ _ = raise NotApplicable "let* needs a list of bindings and a form to evaluate"

and evalDo e (x::xs) = foldl (fn (x, _) => eval e x) (eval e x) xs
  | evalDo _ _ = raise NotApplicable "do needs at least one argument"

and evalIf e [c,a,b] = if truthy (eval e c) then eval e a else eval e b
  | evalIf e [c,a]   = evalIf e [c,a,NIL]
  | evalIf _ _ = raise NotApplicable "if needs two or three arguments"

and evalFn e [LIST (binds,_),body]   = makeFn e binds body
  | evalFn e [VECTOR (binds,_),body] = makeFn e binds body
  | evalFn _ _ = raise NotApplicable "fn* needs a list of bindings and a body"
and makeFn e binds body = FN (fn (exprs) => eval (bind (interleave binds exprs) (inside e)) body, NO_META)

and evalApply e (FN (f,_)) args = f (map (eval e) args)
  | evalApply _ x args = raise NotApplicable (prStr x ^ " is not applicable on " ^ prStr (LIST (args, NO_META)))

and evalSymbol e s = valOrElse (lookup e s)
                               (fn _ => raise NotDefined ("symbol '" ^ s ^ "' not found"))

and bindLet args e = bind' (eval e) args e
and bind args e = bind' identity args e
and bind' evl (SYMBOL "&"::v::(SYMBOL s)::vs) e = (def s (LIST (map evl (v::vs), NO_META)) e; e)
  | bind' _   [SYMBOL "&", SYMBOL s]          e = (def s (LIST ([], NO_META)) e; e)
  | bind' evl (SYMBOL s::v::rest)             e = (def s (evl v) e; bind' evl rest e)
  | bind' _   []                              e = e
  | bind' _ _ _ = raise NotApplicable "bindings must be a list of symbol/form pairs"

fun print f =
    prReadableStr f

fun rep e s =
    s |> read |> eval e |> print
    handle Nothing           => ""
         | SyntaxError msg   => "SYNTAX ERROR: " ^ msg
         | NotApplicable msg => "CANNOT APPLY: " ^ msg
         | NotDefined msg    => "NOT DEFINED: " ^ msg

val replEnv = ENV (NS (ref [])) |> bind coreNs

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
    rep replEnv ("(do " ^ prelude ^ " nil)");
    repl replEnv
)
