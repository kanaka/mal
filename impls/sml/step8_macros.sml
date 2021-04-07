fun read s =
    readStr s

fun eval e ast = eval' e (expandMacro e [ast])

and eval' e (LIST (a::args,_)) = (case specialEval a of SOME special => special e args | _ => evalApply e (eval e a) args)
  | eval' e (SYMBOL s)         = evalSymbol e s
  | eval' e (VECTOR (v,_))     = VECTOR (map (eval e) v, NO_META)
  | eval' e (MAP (m,_))        = MAP (List.map (fn (k, v) => (eval e k, eval e v)) m, NO_META)
  | eval' e ast                = ast

and specialEval (SYMBOL "def!")             = SOME evalDef
  | specialEval (SYMBOL "let*")             = SOME evalLet
  | specialEval (SYMBOL "do")               = SOME evalDo
  | specialEval (SYMBOL "if")               = SOME evalIf
  | specialEval (SYMBOL "fn*")              = SOME evalFn
  | specialEval (SYMBOL "quote")            = SOME evalQuote
  | specialEval (SYMBOL "quasiquote")       = SOME evalQuasiquote
  | specialEval (SYMBOL "quasiquoteexpand") = SOME (fn _ => expandQuasiquote)
  | specialEval (SYMBOL "defmacro!")        = SOME evalDefmacro
  | specialEval (SYMBOL "macroexpand")      = SOME expandMacro
  | specialEval _                           = NONE

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

and evalQuote e [x] = x
  | evalQuote _ _ = raise NotApplicable "quote needs one argument"

and evalQuasiquote e args = eval e (expandQuasiquote args)

and expandQuasiquote [LIST ([SYMBOL "unquote", x],_)] = x
  | expandQuasiquote [LIST (l,_)]    = malList (foldr quasiFolder [] l)
  | expandQuasiquote [VECTOR (v,_)]  = malList [SYMBOL "vec", malList (foldr quasiFolder [] v)]
  | expandQuasiquote [m as MAP _]    = malList [SYMBOL "quote", m]
  | expandQuasiquote [s as SYMBOL _] = malList [SYMBOL "quote", s]
  | expandQuasiquote [x]             = x
  | expandQuasiquote _ = raise NotApplicable "quasiquote needs one argument"
and quasiFolder (LIST ([SYMBOL "splice-unquote", x],_), acc) = [SYMBOL "concat", x, malList acc]
  | quasiFolder (x, acc)                                     = [SYMBOL "cons", expandQuasiquote [x], malList acc]

and evalDefmacro e [SYMBOL s, ast] = defMacro e s (eval e ast)
  | evalDefmacro _ _ = raise NotApplicable "defmacro! needs a name, and a fn*"
and defMacro e s (FN (f,_)) = let val m = MACRO f in (def s m e; m) end
  | defMacro _ _ _ = raise NotApplicable "defmacro! needs a name, and a fn*"

and expandMacro e [(ast as LIST (SYMBOL s::args, _))] = (case lookup e s of SOME (MACRO m) => m args | _ => ast)
  | expandMacro _ [ast]                               = ast
  | expandMacro _ _ = raise NotApplicable "macroexpand needs one argument"

and evalApply e (FN (f,_)) args = f (map (eval e) args)
  | evalApply _ x args = raise NotApplicable (prStr x ^ " is not applicable on " ^ prStr (malList args))

and evalSymbol e s = valOrElse (lookup e s)
                               (fn _ => raise NotDefined ("symbol '" ^ s ^ "' not found"))

and bindLet args e = bind' (eval e) args e
and bind args e = bind' identity args e
and bind' evl (SYMBOL "&"::v::(SYMBOL s)::vs) e = (def s (malList (map evl (v::vs))) e; e)
  | bind' _   [SYMBOL "&", SYMBOL s]          e = (def s (malList []) e; e)
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
         | e                 => "ERROR: " ^ (exnMessage e)

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

val prelude = "                                                \
\\
\(def! not (fn* (a) (if a false true)))                        \
\\
\(def!                                                         \
\  load-file                                                   \
\  (fn* (f)                                                    \
\    (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))\
\\
\(defmacro!                                                    \
\  cond                                                        \
\  (fn* (& xs)                                                 \
\    (if (> (count xs) 0)                                      \
\        (list 'if (first xs)                                  \
\                (if (> (count xs) 1)                          \
\                  (nth xs 1)                                  \
\                  (throw \"odd number of forms to cond\"))    \
\                (cons 'cond (rest (rest xs)))))))"

fun main () = (
    bind [
        SYMBOL "eval",
        FN (fn ([x]) => eval replEnv x
             | _ => raise NotApplicable "'eval' requires one argument", NO_META)
    ] replEnv;
    rep replEnv ("(do " ^ prelude ^ " nil)");
    case CommandLine.arguments () of
        prog::args => (
            def "*ARGV*" (malList (map STRING args)) replEnv;
            rep replEnv ("(load-file \"" ^ prog ^ "\")");
            ()
        )
        | args => (
            def "*ARGV*" (malList (map STRING args)) replEnv;
            repl replEnv
        )
)
