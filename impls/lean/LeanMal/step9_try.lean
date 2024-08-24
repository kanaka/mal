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
    | Except.error (newref, e) => Except.error (newref, e)
    | Except.ok (ref2, fn) => evalFuncVal ref2 fn args

  partial def evalFuncVal (ref: Dict) (fn: Types) (args: List Types) : Except (Dict × String) (Dict × Types) :=
    match fn with
      | Types.funcVal v    => match v with
        | Fun.builtin name =>
          match evalFuncArgs ref args with
          | Except.error e => Except.error e
          | Except.ok (newRef, results) =>
            evalFnNative newRef name results args
        | Fun.userDefined fref params body =>
          match evalFuncArgs ref args with
          | Except.error e => Except.error e
          | Except.ok (newRef, results) =>
            let allkeys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let (keys, variadic) := splitOnAmpersand allkeys
            let normalArgs := results.take keys.length
            let variadicArg := results.drop keys.length
            let argVals := normalArgs ++ [Types.listVal variadicArg]
            let merged := mergeDicts newRef (mergeDicts fref (buildDict (keys ++ variadic) argVals))

            evalTypes merged body
        | Fun.macroFn fref params body =>
          let allkeys: List String := match params with
            | Types.listVal v => v.map fun x => x.toString false
            | _               => []
          let (keys, variadic) := splitOnAmpersand allkeys
          let normalArgs := args.take keys.length
          let variadicArg := args.drop keys.length
          let argVals := normalArgs ++ [Types.listVal variadicArg]
          let merged := mergeDicts ref (mergeDicts fref (buildDict (keys ++ variadic) argVals))

          match evalTypes merged body with
          | Except.error e => Except.error e
          | Except.ok (_, newast) => evalTypes ref newast
      | _ => Except.error (ref, s!"`unexpected token, expected: function`")

  partial def evalList (ref: Dict) (lst : List Types) : Except (Dict × String) (Dict × Types) :=
    if List.length lst == 0 then Except.ok (ref, Types.listVal lst)
    else
      let head := lst[0]!
      match head with
      | Types.symbolVal v => match v with
        | "def!" => evalDefn ref (lst.drop 1)
        | "let*" => evalLet ref (lst.drop 1)
        | "do" => evalDo ref (lst.drop 1)
        | "if" => evalIf ref (lst.drop 1)
        | "fn*" => makeFn ref (lst.drop 1)
        | "try*" => evalTry ref (lst.drop 1)
        | "quote" =>
          if lst.length < 2 then Except.error (ref, "quote: expected 1 argument")
          else Except.ok (ref, lst[1]!)
        | "quasiquote" =>
          if lst.length < 2 then Except.error (ref, "quasiquote: expected 1 argument")
          else evalTypes ref (quasiquote lst[1]!)
        | "defmacro!" => evalDefMacro ref (lst.drop 1)
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

  partial def evalDefMacro (ref: Dict) (args : List Types) : Except (Dict × String) (Dict × Types) :=
    if args.length < 2 then Except.error (ref, "def! unexpected syntax")
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error (newref, e) => Except.error (newref, s!"def!: {e}")
      | Except.ok (newRef, value) =>
        match key with
        | Types.symbolVal v =>
          match value with
          | Types.funcVal func =>
            match func with
            | Fun.macroFn _ _ _ =>
              let refResult := addEntry newRef (KeyType.strKey v) value
              Except.ok (refResult, value)
            | Fun.userDefined fref params body =>
              let refResult := addEntry newRef (KeyType.strKey v) (Types.funcVal (Fun.macroFn fref params body))
              Except.ok (refResult, value)
            | _ => Except.error (newRef, s!"defmacro!: unexpected builtin function")
          | x => Except.error (newRef, s!"unexpected token type: {x.toString true}, expected: function")
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

  partial def evalTry (ref: Dict) (lst : List Types) : Except (Dict × String) (Dict × Types) :=
    if lst.length < 1 then Except.error (ref, "try*: unexpected syntax")
    else
      match evalTypes ref lst[0]! with
      | Except.ok (newRef, result) => Except.ok (newRef, result)
      | Except.error evalErr =>
        if lst.length < 2 then Except.error evalErr
        else
          match lst[1]! with
          | Types.listVal catchBody =>
            if catchBody.length < 1 then Except.error (ref, "try*: unexpected syntax")
            else
              match catchBody[0]! with
              | Types.symbolVal catchSymbol =>
                if catchSymbol == "catch*" then
                  if catchBody.length < 2 then Except.error (ref, "try*: unexpected syntax")
                  else
                    let es := catchBody[1]!
                    match es with
                    | Types.symbolVal errorSymbol =>
                      let (errRef, errStr) := evalErr
                      let err := Types.strVal errStr
                      if catchBody.length < 2 then Except.error (errRef, "try*: unexpected syntax")
                      else
                        let toeval := catchBody[2]!
                        let built := buildDictWithSymbols ref [errorSymbol] [err]
                        let merged := mergeDicts ref built
                        evalTypes merged toeval
                    | _ => Except.error (ref, s!"unexpected return type, expected: symbol")
                else Except.error evalErr
              | _ => Except.error evalErr
          -- | Types.vecVal v => -- TODO
          | _ => Except.error evalErr

  partial def swapAtom (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "swap!: >= 2 argument required")
  else
    let first := lst[0]!
    let fn := lst[1]!
    let rest := lst.drop 2
    match fn with
    | Types.funcVal _ =>
      match first with
      | Types.atomVal x => match x with
        | Atom.v v =>
          match evalFuncVal ref fn ([v] ++ rest) with
          | Except.error (newRef, e) => Except.error (newRef, s!"swap! evaluate function: {e}")
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, Types.atomVal (Atom.v res))
        | Atom.withmeta v meta =>
          match evalFuncVal ref fn ([v] ++ rest) with
          | Except.error (newRef, e) => Except.error (newRef, s!"swap! evaluate function: {e}")
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, Types.atomVal (Atom.withmeta res meta))
      | x => Except.error (ref, s!"deref: unexpected symbol: {x.toString true}, expected: atom")
    | x => Except.error (ref, s!"deref: unexpected symbol: {x.toString true}, expected: function")

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

  partial def nativeMapFnApply (ref: Dict) (fn: Types) (args: List Types) : Except (Dict × String) (Dict × Types) :=
    match args.foldl (fun (res : Except (Dict × String) (Dict × List Types)) x =>
      match res with
      | Except.error e => Except.error e
      | Except.ok (r, acc) =>
        match evalFuncVal r fn [x] with
        | Except.error e => Except.error e
        | Except.ok (updatedRef, res) =>
          Except.ok (updatedRef, acc ++ [res])
    ) (Except.ok (ref, [])) with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) => Except.ok (newRef, Types.listVal results)

  partial def nativeMap (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
    if lst.length < 2 then Except.error (ref, "map: unexpected syntax")
    else
      let fn := lst[0]!
      let params := lst[1]!
      match fn with
      | Types.funcVal _ =>
        match params with
        | Types.listVal v => nativeMapFnApply ref fn v
        | Types.vecVal v => nativeMapFnApply ref fn (toList v)
        | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")
      | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: function")

  partial def nativeApply (ref: Dict) (lst : List Types) : Except (Dict × String) (Dict × Types) :=
    if lst.length < 2 then Except.error (ref, "apply: unexpected syntax")
    else
      let fn := lst[0]!
      let vecargs := lst[lst.length-1]!
      let n := lst.length-2
      let firstargs := lst.drop 1 |>.take n
      match vecargs with
        | Types.listVal v =>
          evalFuncVal ref fn (firstargs ++ v)
        | Types.vecVal v =>
          evalFuncVal ref fn (firstargs ++ (toList v))
        | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")

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
      | "map" => nativeMap ref results
      | "apply" => nativeApply ref results
      | "vec" => makeVec ref results
      | "vector" => makeVector ref results
      | "nth" => nthSeq ref results
      | "first" => firstSeq ref results
      | "rest" => restSeq ref results
      | "conj" => conj ref results
      | "seq" => seq ref results
      | "hash-map" => makeDict ref results
      | "assoc" => assocDict ref results
      | "dissoc" => dissocDict ref results
      | "get" => getDict ref results
      | "contains?" => containsDict ref results
      | "keys" => getKeysDict ref results
      | "vals" => getValuesDict ref results
      | "throw" => throwFn ref results
      | "symbol" => makeSymbol ref results
      | "keyword" => makeKeyword ref results
      | "atom" => makeAtom ref results
      | "deref" => derefAtom ref results
      | "reset!" => resetAtom ref results args
      | "swap!" => swapAtom ref results
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
            | Types.Nil => if name == "nil?" then Except.ok (ref, Types.boolVal true) else Except.ok (ref, Types.boolVal false)
            | Types.intVal _ => if name == "number?" then Except.ok (ref, Types.boolVal true) else Except.ok (ref, Types.boolVal false)
            | Types.floatVal _ => if name == "number?" then Except.ok (ref, Types.boolVal true) else Except.ok (ref, Types.boolVal false)
            | Types.strVal _ => if name == "string?" then Except.ok (ref, Types.boolVal true) else Except.ok (ref, Types.boolVal false)
            | Types.symbolVal _ => if name == "symbol?" then Except.ok (ref, Types.boolVal true) else Except.ok (ref, Types.boolVal false)
            | Types.keywordVal _ => if name == "keyword?" then Except.ok (ref, Types.boolVal true) else Except.ok (ref, Types.boolVal false)
            | Types.dictVal _ => if name == "map?" then Except.ok (ref, Types.boolVal true) else Except.ok (ref, Types.boolVal false)
            | Types.listVal x => match name with
              | "list?" => Except.ok (ref, Types.boolVal true)
              | "sequential?" => Except.ok (ref, Types.boolVal true)
              | "empty?" => Except.ok (ref, Types.boolVal (x.length == 0))
              | _ => Except.ok (ref, Types.boolVal false)
            | Types.vecVal x => match name with
              | "sequential?" => Except.ok (ref, Types.boolVal true)
              | "vector?" => Except.ok (ref, Types.boolVal false)
              | "empty?" => Except.ok (ref, Types.boolVal ((toList x).length == 0))
              | _ => Except.ok (ref, Types.boolVal false)
            | Types.boolVal x => match name with
              | "true?" => Except.ok (ref, Types.boolVal x)
              | "false?" => Except.ok (ref, Types.boolVal !x)
              | _ => Except.ok (ref, Types.boolVal false)
            | Types.atomVal _ => match name with
              | "atom?" => Except.ok (ref, Types.boolVal true)
              | _ => Except.ok (ref, Types.boolVal false)
            | Types.funcVal func => match name with
              | "fn?" => Except.ok (ref, Types.boolVal true)
              | "macro?" => match func with
                | Fun.builtin _ => Except.ok (ref, Types.boolVal false)
                | Fun.userDefined _ _ _ => Except.ok (ref, Types.boolVal false)
                | Fun.macroFn _ _ _ =>  Except.ok (ref, Types.boolVal true)
              | _ => Except.ok (ref, Types.boolVal false)
          | _   => Except.error (ref, s!"'{name}' not found")

end

def READ (input : String): Except String Types :=
  read_str.{u} input

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (ref: Dict) (input : String): Dict × String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes ref result with
    | Except.error (newref, e) => (newref, s!"Error: {e}")
    | Except.ok (newref, res) => (newref, PRINT res)
  | Except.error err => (ref, s!"Parsing failed: {err}")

def printLogs (ref : Dict) : IO Unit :=
  forM (getLogsInfo ref) (fun elem =>
    match elem with
    | Types.strVal log => IO.println log
    | x => IO.println (x.toString true)
  )

def loadMalFns (ref: Dict) (fndefs: List String): Dict × String :=
  fndefs.foldl (fun (res : Dict × String) fndef =>
    let (ref, msg) := res
    let (newref, newmsg) := rep.{u} ref fndef
    (newref, s!"{msg}¬{newmsg}")
  ) (ref, "")

def fnDefs: List String := [
    "(def! *host-language* \"Lean\")",
    "(def! not (fn* (a) (if a false true)))"
  ]

def main : IO Unit := do
  IO.println "Welcome to Mal REPL!"
  let (env0, _) := loadMalFns.{u} (loadFnNativeAll Dict.empty) fnDefs
  let mut env := env0

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
