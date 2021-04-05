fun read s =
    readStr s

fun eval e ast = eval' e (expandMacro e [ast])

and eval' e (LIST (SYMBOL "def!"::args))             = evalDef e args
  | eval' e (LIST (SYMBOL "let*"::args))             = evalLet e args
  | eval' e (LIST (SYMBOL "do"::args))               = evalDo e args
  | eval' e (LIST (SYMBOL "if"::args))               = evalIf e args
  | eval' e (LIST (SYMBOL "fn*"::args))              = evalFn e args
  | eval' e (LIST (SYMBOL "quote"::args))            = evalQuote e args
  | eval' e (LIST (SYMBOL "quasiquote"::args))       = eval e (expandQuasiquote args)
  | eval' e (LIST (SYMBOL "quasiquoteexpand"::args)) = expandQuasiquote args
  | eval' e (LIST (SYMBOL "defmacro!"::args))        = evalDefmacro e args
  | eval' e (LIST (SYMBOL "macroexpand"::args))      = expandMacro e args
  | eval' e (LIST (a::args))                         = evalApply e (eval e a) args
  | eval' e (SYMBOL s)                               = evalSymbol e s
  | eval' e (VECTOR v)                               = VECTOR (map (eval e) v)
  | eval' e (MAP m)                                  = MAP (List.map (fn (k, v) => (eval e k, eval e v)) m)
  | eval' e ast                                      = ast

and evalDef e [SYMBOL s, ast] = let val v = eval e ast in (def s v e; v) end
  | evalDef _ _               = raise NotApplicable "def! needs a symbol and a form to evaluate"

and evalLet e [LIST bs, ast]   = let val e = inside e in eval (bind (eval e) bs e) ast end
  | evalLet e [VECTOR bs, ast] = let val e = inside e in eval (bind (eval e) bs e) ast end
  | evalLet _ _                = raise NotApplicable "let* needs a list of bindings and a form to evaluate"

and evalDo e (x::xs) = foldl (fn (x, _) => eval e x) (eval e x) xs
  | evalDo _ _       = raise NotApplicable "do needs at least one argument"

and evalIf e [c,a,b] = if truthy (eval e c) then eval e a else eval e b
  | evalIf e [c,a]   = evalIf e [c,a,NIL]
  | evalIf _ _       = raise NotApplicable "if needs two or three arguments"

and evalFn e [(LIST binds),body]   = makeFn e binds body
  | evalFn e [(VECTOR binds),body] = makeFn e binds body
  | evalFn _ _                     = raise NotApplicable "fn* needs a list of bindings and a body"
and makeFn e binds body = FN (fn (exprs) => eval (bind (eval e) (interleave binds exprs) (inside e)) body)

and evalQuote e [x] = x
  | evalQuote _ _   = raise NotApplicable "quote needs one argument"

and expandQuasiquote [LIST [SYMBOL "unquote", x]] = x
  | expandQuasiquote [LIST l]        = LIST (foldr quasiFolder [] l)
  | expandQuasiquote [VECTOR v]      = LIST [SYMBOL "vec", LIST (foldr quasiFolder [] v)]
  | expandQuasiquote [m as MAP _]    = LIST [SYMBOL "quote", m]
  | expandQuasiquote [s as SYMBOL _] = LIST [SYMBOL "quote", s]
  | expandQuasiquote [x]             = x
  | expandQuasiquote _               = raise NotApplicable "quasiquote needs one argument"
and quasiFolder (LIST [SYMBOL "splice-unquote", x], acc) = [SYMBOL "concat", x, LIST acc]
  | quasiFolder (x, acc)                                 = [SYMBOL "cons", expandQuasiquote [x], LIST acc]

and evalDefmacro e [SYMBOL s, LIST [SYMBOL "fn*", LIST binds, body]]   = let val m = makeMacro e binds body in (def s m e; m) end
  | evalDefmacro e [SYMBOL s, LIST [SYMBOL "fn*", VECTOR binds, body]] = let val m = makeMacro e binds body in (def s m e; m) end
  | evalDefmacro _ _ = raise NotApplicable "defmacro! needs a name, a list of bindings, and a body"
and makeMacro e binds body = MACRO (fn (exprs) => eval (bind identity (interleave binds exprs) (inside e)) body)

and expandMacro e [(ast as LIST (SYMBOL s::args))] = (case lookup e s of SOME (MACRO m) => m args | _ => ast)
  | expandMacro _ [ast]                            = ast
  | expandMacro _ _ = raise NotApplicable "macroexpand needs one argument"

and evalApply e (FN f) args = f (map (eval e) args)
  | evalApply _ x      args = raise NotApplicable (prStr x ^ " is not applicable on " ^ prStr (LIST args))

and evalSymbol e s = valOrElse (lookup e s)
                               (fn _ => raise NotDefined ("symbol '" ^ s ^ "' not found"))

and bind evl (SYMBOL "&"::v::(SYMBOL s)::vs) e = (def s (LIST (map evl (v::vs))) e; e)
  | bind _   [SYMBOL "&", SYMBOL s]          e = (def s (LIST []) e; e)
  | bind evl (SYMBOL s::v::rest)             e = (def s (evl v) e; bind evl rest e)
  | bind _   []                              e = e
  | bind _ _ _ = raise NotApplicable "bindings must be a list of symbol/form pairs"

fun print f =
    prReadableStr f

fun rep e s =
    s |> read |> eval e |> print
    handle Nothing           => ""
         | SyntaxError msg   => "SYNTAX ERROR: " ^ msg
         | NotApplicable msg => "CANNOT APPLY: " ^ msg
         | NotDefined msg    => "NOT DEFINED: " ^ msg
         | e                 => "ERROR: " ^ (exnMessage e)

val initEnv = ENV (NS (ref [])) |> bind identity coreNs

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
    bind identity [
        SYMBOL "eval",
        FN (fn ([x]) => eval initEnv x
             | _ => raise NotApplicable "'eval' requires one argument")
    ] initEnv;
    rep initEnv ("(do " ^ prelude ^ " nil)");
    case CommandLine.arguments () of
        prog::args => (
            def "*ARGV*" (LIST (map STRING args)) initEnv;
            rep initEnv ("(load-file \"" ^ prog ^ "\")");
            ()
        )
        | args => (
            def "*ARGV*" (LIST (map STRING args)) initEnv;
            repl initEnv
        )
)
