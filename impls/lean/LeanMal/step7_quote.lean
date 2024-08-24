import LeanMal.reader
import LeanMal.printer
import LeanMal.core

universe u

def makeFn (ref: Dict) (args : List Types) : Except (Dict × String) (Dict × Types) :=
  if args.length < 2 then Except.error (ref, "unexpected syntax")
  else
    let params := args[0]!
    let body := args[1]!
    let newfn := Fun.userDefined ref params body
    Except.ok (ref, Types.funcVal newfn)

def splitOnAmpersand (input : List String) : (List String × List String) :=
  let rec loop (acc1 : List String) (rest : List String) : (List String × List String) :=
    match rest with
    | []         => (acc1, [])  -- If no "&" found, second list is empty
    | "&" :: xs  => match xs with
      | [] => (acc1, [])  -- If "&" is the last element, second list is empty
      | y :: _ => (acc1, [y])  -- Add the next element after "&" to the second list
    | x :: xs    => loop (acc1 ++ [x]) xs  -- Accumulate elements before "&"
  loop [] input

mutual
  partial def evalTypes (_ref : Dict := Dict.empty) (ast : Types) : Except (Dict × String) (Dict × Types) :=
    let ref := if getDebugEval _ref then logInfo _ref s!"EVAL:{pr_str true ast}"
      else _ref
    match ast with
    | Types.symbolVal v   => match getEntry ref (KeyType.strKey v) with
      | some vi => Except.ok (ref, vi)
      | none => Except.error (ref, s!"'{v}' not found")
    | Types.listVal el    => (evalList ref el)
    | Types.vecVal el     => (evalVec ref (toList el))
    | Types.dictVal el    => (evalDict ref el)
    | x                   => Except.ok (ref, x)

  partial def evalFunc (ref: Dict) (head : Types) (args : List Types) : Except (Dict × String) (Dict × Types) :=
    match evalTypes ref head with
    | Except.error (newref, e) => Except.error (newref, s!"error evaluating function: {head.toString true}: {e}")
    | Except.ok (ref2, fn) => evalFuncVal ref2 fn args

  partial def evalFuncVal (ref: Dict) (fn: Types) (args: List Types) : Except (Dict × String) (Dict × Types) :=
    -- first execute each function argument - reduce computation
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) =>
      match fn with
        | Types.funcVal v      => match v with
          | Fun.builtin name =>
            evalFnNative newRef name results args
          | Fun.userDefined fref params body =>
            let allkeys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let (keys, variadic) := splitOnAmpersand allkeys
            let normalArgs := results.take keys.length
            let variadicArg := results.drop keys.length
            let argVals := normalArgs ++ [Types.listVal variadicArg]
            let argsDict := (buildDict (keys ++ variadic) argVals)
            let merged := mergeDicts (mergeDicts newRef fref) argsDict

            evalTypes merged body
          | Fun.macroFn _ _ _ => Except.error (newRef, "macro not implemented")
        | _ => Except.error (newRef, s!"`unexpected token, expected: function`")

  partial def evalList (ref: Dict) (lst : List Types) : Except (Dict × String) (Dict × Types) :=
    if List.length lst == 0 then Except.ok (ref, Types.listVal lst)
    else
      let head := lst[0]!
      match lst[0]! with
      | Types.symbolVal v => match v with
        | "def!" => evalDefn ref (lst.drop 1)
        | "let*" => evalLet ref (lst.drop 1)
        | "do" => evalDo ref (lst.drop 1)
        | "if" => evalIf ref (lst.drop 1)
        | "fn*" => makeFn ref (lst.drop 1)
        | "quote" =>
          if lst.length < 2 then Except.error (ref, "quote: expected 1 argument")
          else Except.ok (ref, lst[1]!)
        | "quasiquote" =>
          if lst.length < 2 then Except.error (ref, "quasiquote: expected 1 argument")
          else evalTypes ref (quasiquote lst[1]!)
        | _ => evalFunc ref head (lst.drop 1)
      | _ => evalFunc ref head (lst.drop 1)

  partial def evalVec (ref: Dict) (elems : List Types) : Except (Dict × String) (Dict × Types) :=
    match evalFuncArgs ref elems with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) => Except.ok (newRef, Types.vecVal (listToVec results))

  partial def evalDict (ref: Dict) (lst : Dict) : Except (Dict × String) (Dict × Types) :=
    match evalDictInner ref lst with
      | Except.error e => Except.error e
      | Except.ok (newRef, newDict) => Except.ok (newRef, Types.dictVal newDict)

  partial def evalDictInner (ref: Dict) (lst : Dict) : Except (Dict × String) (Dict × Dict) :=
    match lst with
      | Dict.empty => Except.ok (ref, lst)
      | Dict.insert k v restDict => match evalTypes ref v with
        | Except.error e => Except.error e
        | Except.ok (newRef, newVal) => match evalDictInner newRef restDict with
          | Except.error e => Except.error e
          | Except.ok (updatedRef, updatedDict) =>
            let newDict := Dict.insert k newVal updatedDict
            Except.ok (updatedRef, newDict)

  partial def evalFuncArgs (ref: Dict) (args: List Types) : Except (Dict × String) (Dict × List Types) :=
    match args.foldl (fun (res : Except (Dict × String) (Dict × List Types)) x =>
        match res with
        | Except.error (newref, e) => Except.error (newref, s!"error evaluating function argument accumulator: {x.toString true}: {e}")
        | Except.ok (r, acc) => match evalTypes r x with
          | Except.error (newref, e) => Except.error (newref, s!"error evaluating function argument: {x.toString true}: {e}")
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, acc ++ [res])
      ) (Except.ok (ref, [])) with
      | Except.error e => Except.error e
      | Except.ok (newRef, results) => Except.ok (newRef, results)

  partial def evalDefn (ref: Dict) (args : List Types) : Except (Dict × String) (Dict × Types) :=
    if args.length < 2 then Except.error (ref, "def! unexpected syntax")
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error (newref, e) => Except.error (newref, s!"def!: {e}")
      | Except.ok (newRef, value) =>
        match key with
        | Types.symbolVal v =>
          let refResult := addEntry newRef (KeyType.strKey v) value
          Except.ok (refResult, value)
        | _ => Except.error (newRef, s!"def! unexpected token, expected: symbol")

  partial def evalLet (ref: Dict) (args : List Types) : Except (Dict × String) (Dict × Types) :=
    if args.length < 2 then Except.error (ref, "let*: unexpected syntax")
    else
      let pairs := args[0]!
      let body := args[1]!
      let result := match pairs with
      | Types.listVal v => evalLetArgs ref v
      | Types.vecVal v => evalLetArgs ref (toList v)
      | _ => Except.error (ref, s!"unexpected token type: ${pairs.toString true}, expected: list or vector")

      match result with
      | Except.error (newRef, e) => Except.error (newRef, s!"let*: {e}")
      | Except.ok newRef => match evalTypes newRef body with
        | Except.error e => Except.error e
        -- we do not propagate the let* environment to the parent scope
        | Except.ok (_, result) => Except.ok (ref, result)

  partial def evalLetArgs (ref: Dict) (args : List Types) : Except (Dict × String) Dict :=
    match args with
    | [] => Except.ok ref
    | [_] => Except.error (ref, "let*: unexpected syntax")
    | x :: y :: rest =>
      match x with
      | Types.symbolVal key => match evalTypes ref y with
        | Except.error (newRef, e) => Except.error (newRef, s!"error evaluating function argument: {key}: {e}")
        | Except.ok (updatedRef, value) =>
          evalLetArgs (addEntry updatedRef (KeyType.strKey key) value) rest
      | _ => Except.error (ref, "let*: unexpected syntax")

  partial def evalDo (ref: Dict) (args : List Types) : Except (Dict × String) (Dict × Types) :=
    -- only return last computation result
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) =>
      if results.length == 0 then Except.ok (newRef, Types.Nil)
      else Except.ok (newRef, results[results.length - 1]!)

  partial def evalIf (ref: Dict) (args : List Types) : Except (Dict × String) (Dict × Types) :=
    if args.length < 2 then Except.error (ref, "unexpected syntax")
    else
      let condition := args[0]!
      let thenExpr := args[1]!
      let hasElse := args.length > 2

      match evalTypes ref condition with
      | Except.error (newRef, e) => Except.error (newRef, s!"if: {e}")
      | Except.ok (newRef, condResp) =>
        let cond := match condResp with
        | Types.boolVal v => v
        | Types.Nil => false
        | _ => true
        if cond then evalTypes newRef thenExpr
        else if hasElse then evalTypes newRef args[2]!
        else Except.ok (newRef, Types.Nil)

  partial def swapAtom (ref: Dict) (lst: List Types) (args: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "swap!: >= 2 argument required")
  else
    let first := lst[0]!
    let fn := lst[1]!
    let rest := lst.drop 2
    match args[0]! with
    | Types.symbolVal sym =>
      match fn with
      | Types.funcVal _ =>
        match first with
        | Types.atomVal x => match x with
          | Atom.v v =>
            match evalFuncVal ref fn ([v] ++ rest) with
            | Except.error (newRef, e) => Except.error (newRef, s!"swap! evaluate function: {e}")
            | Except.ok (updatedRef, res) =>
              let newRef := addEntry updatedRef (KeyType.strKey sym) (Types.atomVal (Atom.v res))
              Except.ok (newRef, res)
          | Atom.withmeta v meta =>
            match evalFuncVal ref fn ([v] ++ rest) with
            | Except.error (newRef, e) => Except.error (newRef, s!"swap! evaluate function: {e}")
            | Except.ok (updatedRef, res) =>
              let newRef := addEntry updatedRef (KeyType.strKey sym) (Types.atomVal (Atom.withmeta res meta))
              Except.ok (newRef, res)
        | x => Except.error (ref, s!"swap!: unexpected symbol: {x.toString true}, expected: atom")
      | x => Except.error (ref, s!"swap!: unexpected symbol: {x.toString true}, expected: function")
    | x => Except.error (ref, s!"swap!: unexpected token: {x.toString true}, expected: symbol")

  partial def eval (ref: Dict) (lst : List Types) : Except (Dict × String) (Dict × Types) :=
    if lst.length < 1 then Except.error (ref, "eval: unexpected syntax")
    else
      let ast := lst[0]!
      evalTypes ref ast

  partial def starts_with (lst: List Types) (symb: String) : Bool :=
    if lst.length == 2 then
      match lst[0]! with
      | Types.symbolVal v => v == symb
      | _ => false
    else false

  partial def qq_loop (elt : Types) (acc: List Types): List Types :=
    match elt with
    | Types.listVal v =>
      if starts_with v "splice-unquote"
      then
        [Types.symbolVal "concat", v[1]!, Types.listVal acc]
      else
        [Types.symbolVal "cons", quasiquote elt, Types.listVal acc]
    | _ => [Types.symbolVal "cons", quasiquote elt, Types.listVal acc]

  partial def qq_foldr (lst : List Types): Types :=
    let res := lst.reverse.foldl (fun acc x => qq_loop x acc) []
    Types.listVal res

  partial def quasiquote (ast: Types) : Types :=
    match ast with
    | Types.symbolVal _ => Types.listVal [Types.symbolVal "quote", ast]
    | Types.dictVal _ => Types.listVal [Types.symbolVal "quote", ast]
    | Types.listVal v =>
      if starts_with v "unquote" then v[1]!
      else qq_foldr v
    | Types.vecVal v => Types.listVal [Types.symbolVal "vec", qq_foldr (toList v)]
    | _ => ast

  partial def evalFnNative (ref : Dict := Dict.empty) (name: String) (results: List Types) (args: List Types): Except (Dict × String) (Dict × Types) :=
    match name with
    | "+" => sum ref results
    | "-" => sub ref results
    | "*" => mul ref results
    | "/" => div ref results
    | "<" => lt ref results
    | "<=" => lte ref results
    | ">" => gt ref results
    | ">=" => gte ref results
    | "=" => eq ref results false
    | "list" => Except.ok (ref, Types.listVal results)
    | "count" => countFunc ref results
    | "cons" => cons ref results
    | "concat" => concat ref results
    | "vec" => makeVec ref results
    | "atom" => makeAtom ref results
    | "deref" => derefAtom ref results
    | "reset!" => resetAtom ref results args
    | "swap!" => swapAtom ref results args
    | "prn" => prnFunc ref results
    | "pr-str" => prStrFunc ref results
    | "str" => strFunc ref results
    | "println" => printlnFunc ref results
    | "eval" => eval ref results
    | "read-string" => match readString results ref with -- readString results Dict.empty
      | Except.error e => Except.error (ref, e)
      | Except.ok res => Except.ok (ref, res)
    | _ => match results with
        | [x] => match x with
          | Types.listVal x => match name with
            | "list?" => Except.ok (ref, Types.boolVal true)
            | "empty?" => Except.ok (ref, Types.boolVal (x.length == 0))
            | _ => Except.ok (ref, Types.boolVal false)
          | Types.atomVal _ => match name with
            | "atom?" => Except.ok (ref, Types.boolVal true)
            | _ => Except.ok (ref, Types.boolVal false)
          | _   => Except.ok (ref, Types.boolVal false)
        | _   => Except.error (ref, s!"'{name}' not found")

end

def READ (input : String): Except String Types :=
  read_str.{u} input

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (ref: Dict) (input : String): Dict × String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes ref result with
    | Except.error (newref, e) => (newref, e)
    | Except.ok (newref, res) => (newref, PRINT res)
  | Except.error err => (ref, s!"Parsing failed: {err}")

def printLogs (ref : Dict) : IO Unit :=
  forM (getLogsInfo ref) (fun elem =>
    match elem with
    | Types.strVal log => IO.println log
    | x => IO.println (x.toString true)
  )

def main : IO Unit := do
  IO.println "Welcome to Mal REPL!"
  let mut env := loadFnNativeAll Dict.empty

  let mut donext := true
  while donext do
    IO.print "user> "
    let stdin ← IO.getStdin
    let input ← stdin.getLine
    let value := input.trim
    if value = "exit" then
      donext := false
      IO.println "Exiting REPL."
    if value.isEmpty then
      donext := false
    else
      let (ref, val) := rep.{u} env value
      printLogs ref
      IO.println val
      env := resetLogs ref
