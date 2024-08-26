import LeanMal.reader
import LeanMal.printer
import LeanMal.core

universe u

def makeFn (env: Env) (args : List Types) : IO (Env × Types) := do
  if args.length < 2 then throw (IO.userError "unexpected syntax")
  else
    let p := args[0]!
    let body := args[1]!
    let params := match p with
      | Types.vecVal x => Types.listVal (toList x)
      | _ => p
    let newfn := Fun.userDefined ref.increment params body
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
  partial def evalTypes (_ref : Env) (ast : Types) : IO (Env × Types) := do
    let ref := if getDebugEval _ref then logInfo _ref s!"EVAL:{pr_str true ast}"
      else _ref
    match ast with
    | Types.symbolVal v   => match ref.get (KeyType.strKey v) with
      | some (_, vi) => Except.ok (ref, vi)
      | none => throw (IO.userError s!"'{v}' not found")
    | Types.listVal el    => (evalList ref el)
    | Types.vecVal el     => (evalVec ref (toList el))
    | Types.dictVal el    => (evalDict ref el)
    | x                   => Except.ok (ref, x)

   partial def evalFunc (env: Env) (head : Types) (args : List Types) : IO (Env × Types) := do
    match evalTypes ref head with
    | Except.error e => Except.error e
    | Except.ok (ref2, fn) =>
      match evalFuncVal ref2 fn args true with
      | Except.error e => Except.error e
      | Except.ok (fref, res) =>
        -- after executing a function, propagate atoms (defined in outer environments) and logs to the parent scope
        Except.ok (forwardLogs fref (forwardMutatedAtoms fref ref), res)

  partial def evalFuncVal (env: Env) (fn: Types) (args: List Types) (evaluateArgs: Bool) : IO (Env × Types) := do
    match fn with
      | Types.funcVal v    => match v with
        | Fun.builtin name =>
          match if !evaluateArgs then Except.ok (ref, args) else
            match evalFuncArgs ref args with
              | Except.error e => Except.error e
              | Except.ok (newEnv, results) => Except.ok (newEnv, results)
          with
            | Except.error e => Except.error e
            | Except.ok (newEnv, results) => evalFnNative newEnv name results args
        | Fun.userDefined fref params body =>
           match if !evaluateArgs then Except.ok (ref, args) else
            match evalFuncArgs ref args with
              | Except.error e => Except.error e
              | Except.ok (newEnv, results) => Except.ok (newEnv, results)
          with
            | Except.error e => Except.error e
            | Except.ok (newEnv, results) =>
              let allkeys: List String := match params with
                | Types.listVal v => v.map fun x => x.toString false
                | _               => []
              let (keys, variadic) := splitOnAmpersand allkeys
              let normalArgs := results.take keys.length
              let variadicArg := results.drop keys.length
              let argVals := normalArgs ++ [Types.listVal variadicArg]
              let argsLevel := if fref.getLevel >= newEnv.getLevel then fref.getLevel + 1 else newEnv.getLevel + 1

              let argsDict := (buildDict argsLevel (keys ++ variadic) argVals)
              let merged := (newEnv.merge fref).mergeDict argsLevel argsDict

              evalTypes merged body
        | Fun.macroFn fref params body =>
          let allkeys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let (keys, variadic) := splitOnAmpersand allkeys
          let normalArgs := args.take keys.length
          let variadicArg := args.drop keys.length
          let argVals := normalArgs ++ [Types.listVal variadicArg]
          let argsLevel := if fref.getLevel >= ref.getLevel then fref.getLevel + 1 else ref.getLevel + 1
          let argsDict := (buildDict argsLevel (keys ++ variadic) argVals)
          let merged := (ref.merge fref).mergeDict argsLevel argsDict

          match evalTypes merged body with
          | Except.error e => Except.error e
          | Except.ok (_, newast) => evalTypes ref newast
      | _ => throw (IO.userError s!"`unexpected token, expected: function`")

  partial def evalList (env: Env) (lst : List Types) : IO (Env × Types) := do
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
          if lst.length < 2 then throw (IO.userError "quote: expected 1 argument")
          else Except.ok (ref, lst[1]!)
        | "quasiquote" =>
          if lst.length < 2 then throw (IO.userError "quasiquote: expected 1 argument")
          else evalTypes ref (quasiquote lst[1]!)
        | "defmacro!" => evalDefMacro ref (lst.drop 1)
        | _ => evalFunc ref head (lst.drop 1)
      | _ => evalFunc ref head (lst.drop 1)

  partial def evalVec (env: Env) (elems : List Types) : IO (Env × Types) := do
    match evalFuncArgs ref elems with
    | Except.error e => Except.error e
    | Except.ok (newEnv, results) => Except.ok (newEnv, Types.vecVal (listToVec results))

  partial def evalDict (env: Env) (lst : Dict) : IO (Env × Types) := do
    match evalDictInner ref lst with
      | Except.error e => Except.error e
      | Except.ok (newEnv, newDict) => Except.ok (newEnv, Types.dictVal newDict)

  partial def evalDictInner (env: Env) (lst : Dict) : IO (Env × Dict) :=
    match lst with
      | Dict.empty => Except.ok (ref, lst)
      | Dict.insert k _ v restDict => match evalTypes ref v with
        | Except.error e => Except.error e
        | Except.ok (newEnv, newVal) => match evalDictInner newEnv restDict with
          | Except.error e => Except.error e
          | Except.ok (updatedRef, updatedDict) =>
            let newDict := Dict.insert k 0 newVal updatedDict
            Except.ok (updatedRef, newDict)

  partial def evalFuncArgs (env: Env) (args: List Types) : IO (Env × List Types) :=
    match args.foldl (fun (res : IO (Env × List Types)) x =>
        match res with
        | Except.error (newref, e) => Except.error (newref, s!"error evaluating function argument accumulator: {x.toString true}: {e}")
        | Except.ok (r, acc) => match evalTypes r x with
          | Except.error (newref, e) => Except.error (newref, s!"error evaluating function argument: {x.toString true}: {e}")
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, acc ++ [res])
      ) (Except.ok (ref, [])) with
      | Except.error e => Except.error e
      | Except.ok (newEnv, results) => Except.ok (newEnv, results)

  partial def evalDefn (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "def! unexpected syntax")
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error (newref, e) => Except.error (newref, s!"def!: {e}")
      | Except.ok (newEnv, value) =>
        match key with
        | Types.symbolVal v =>
          let refResult := newEnv.add (KeyType.strKey v) ref.getLevel value
          Except.ok (refResult, value)
        | _ => Except.error (newEnv, s!"def! unexpected token, expected: symbol")

  partial def evalDefMacro (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "def! unexpected syntax")
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error (newref, e) => Except.error (newref, s!"def!: {e}")
      | Except.ok (newEnv, value) =>
        match key with
        | Types.symbolVal v =>
          match value with
          | Types.funcVal func =>
            match func with
            | Fun.macroFn _ _ _ =>
              let refResult := newEnv.add (KeyType.strKey v) ref.getLevel value
              Except.ok (refResult, value)
            | Fun.userDefined fref params body =>
              let refResult := newEnv.add (KeyType.strKey v) ref.getLevel (Types.funcVal (Fun.macroFn fref params body))
              Except.ok (refResult, value)
            | _ => Except.error (newEnv, s!"defmacro!: unexpected builtin function")
          | x => Except.error (newEnv, s!"unexpected token type: {x.toString true}, expected: function")
        | _ => Except.error (newEnv, s!"def! unexpected token, expected: symbol")

  partial def evalLet (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "let*: unexpected syntax")
    else
      let pairs := args[0]!
      let body := args[1]!
      let result := match pairs with
      | Types.listVal v => evalLetArgs ref.increment v
      | Types.vecVal v => evalLetArgs ref.increment (toList v)
      | _ => throw (IO.userError s!"unexpected token type: ${pairs.toString true}, expected: list or vector")

      match result with
      | Except.error (newEnv, e) => Except.error (newEnv, s!"let*: {e}")
      | Except.ok newEnv => match evalTypes newEnv body with
        | Except.error e => Except.error e
        -- after executing let*, propagate atoms (defined in outer environments) and logs to the parent scope
        | Except.ok (letref, result) =>
          Except.ok (forwardLogs letref (forwardMutatedAtoms letref ref), result)

  partial def evalLetArgs (env: Env) (args : List Types) : IO Env :=
    match args with
    | [] => Except.ok ref
    | [_] => throw (IO.userError "let*: unexpected syntax")
    | x :: y :: rest =>
      match x with
      | Types.symbolVal key => match evalTypes ref y with
        | Except.error (newEnv, e) => Except.error (newEnv, s!"error evaluating function argument: {key}: {e}")
        | Except.ok (updatedRef, value) =>
          evalLetArgs (updatedRef.add (KeyType.strKey key) ref.getLevel value) rest
      | _ => throw (IO.userError "let*: unexpected syntax")

  partial def evalDo (env: Env) (args : List Types) : IO (Env × Types) := do
    -- only return last computation result
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newEnv, results) =>
      if results.length == 0 then Except.ok (newEnv, Types.Nil)
      else Except.ok (newEnv, results[results.length - 1]!)

  partial def evalIf (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "unexpected syntax")
    else
      let condition := args[0]!
      let thenExpr := args[1]!
      let hasElse := args.length > 2

      match evalTypes ref condition with
      | Except.error (newEnv, e) => Except.error (newEnv, s!"if: {e}")
      | Except.ok (newEnv, condResp) =>
        let cond := match condResp with
        | Types.boolVal v => v
        | Types.Nil => false
        | _ => true
        if cond then evalTypes newEnv thenExpr
        else if hasElse then evalTypes newEnv args[2]!
        else Except.ok (newEnv, Types.Nil)

  partial def evalTry (env: Env) (lst : List Types) : IO (Env × Types) := do
    if lst.length < 1 then throw (IO.userError "try*: unexpected syntax")
    else
      match evalTypes ref lst[0]! with
      | Except.ok (newEnv, result) => Except.ok (newEnv, result)
      | Except.error evalErr =>
        if lst.length < 2 then Except.error evalErr
        else
          match lst[1]! with
          | Types.listVal catchBody =>
            if catchBody.length < 1 then throw (IO.userError "try*: unexpected syntax")
            else
              match catchBody[0]! with
              | Types.symbolVal catchSymbol =>
                if catchSymbol == "catch*" then
                  if catchBody.length < 2 then throw (IO.userError "try*: unexpected syntax")
                  else
                    let es := catchBody[1]!
                    match es with
                    | Types.symbolVal errorSymbol =>
                      let (errRef, errStr) := evalErr
                      let err := Types.strVal errStr
                      if catchBody.length < 2 then Except.error (errRef, "try*: unexpected syntax")
                      else
                        let toeval := catchBody[2]!
                        let built := buildDictWithSymbols ref.getDict ref.getLevel [errorSymbol] [err]
                        let merged := ref.mergeDict (ref.getLevel + 1) built
                        evalTypes merged toeval
                    | _ => throw (IO.userError s!"unexpected return type, expected: symbol")
                else Except.error evalErr
              | _ => Except.error evalErr
          -- | Types.vecVal v => -- TODO
          | _ => Except.error evalErr

  partial def swapAtom (env: Env) (lst: List Types) (args: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "swap!: >= 2 argument required")
  else
    let first := lst[0]!
    let fn := lst[1]!
    let rest := lst.drop 2
    match args[0]! with
    | Types.symbolVal sym =>
      match fn with
      | Types.funcVal _ =>
        match ref.get (KeyType.strKey sym) with
        | none => throw (IO.userError s!"{sym} not found")
        | some (level, _) => match first with
          | Types.atomVal x => match x with
            | Atom.v v =>
              match evalFuncVal ref fn ([v] ++ rest) false with
              | Except.error (newEnv, e) => Except.error (newEnv, s!"swap! evaluate function: {e}")
              | Except.ok (_, res) =>
                let newEnv := ref.add (KeyType.strKey sym) level (Types.atomVal (Atom.v res))
                Except.ok (newEnv, res)
            | Atom.withmeta v meta =>
              match evalFuncVal ref fn ([v] ++ rest) false with
              | Except.error (newEnv, e) => Except.error (newEnv, s!"swap! evaluate function: {e}")
              | Except.ok (_, res) =>
                let newEnv := ref.add (KeyType.strKey sym) level (Types.atomVal (Atom.withmeta res meta))
                Except.ok (newEnv, res)
          | x => throw (IO.userError s!"swap!: unexpected symbol: {x.toString true}, expected: atom")
      | x => throw (IO.userError s!"swap!: unexpected symbol: {x.toString true}, expected: function")
    | x => throw (IO.userError s!"swap!: unexpected token: {x.toString true}, expected: symbol")

  partial def eval (env: Env) (lst : List Types) : IO (Env × Types) := do
    if lst.length < 1 then throw (IO.userError "eval: unexpected syntax")
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

  partial def nativeMapOverList (env: Env) (fn: Types) (args: List Types) : IO (Env × Types) := do
    match args.foldl (fun (res : IO (Env × List Types)) x =>
      match res with
      | Except.error e => Except.error e
      | Except.ok (r, acc) =>
        match evalFuncVal r fn [x] false with
        | Except.error e => Except.error e
        | Except.ok (updatedRef, res) =>
          Except.ok (updatedRef, acc ++ [res])
    ) (Except.ok (ref, [])) with
    | Except.error e => Except.error e
    | Except.ok (newEnv, results) => Except.ok (newEnv, Types.listVal results)

  partial def nativeMap (env: Env) (lst: List Types) : IO (Env × Types) := do
    if lst.length < 2 then throw (IO.userError "map: unexpected syntax")
    else
      let fn := lst[0]!
      let params := lst[1]!
      match fn with
      | Types.funcVal _ =>
        match params with
        | Types.listVal v => nativeMapOverList ref fn v
        | Types.vecVal v => nativeMapOverList ref fn (toList v)
        | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")
      | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: function")

  partial def nativeApply (env: Env) (lst : List Types) : IO (Env × Types) := do
    if lst.length < 2 then throw (IO.userError "apply: unexpected syntax")
    else
      let fn := lst[0]!
      let vecargs := lst[lst.length-1]!
      let n := lst.length-2
      let firstargs := lst.drop 1 |>.take n
      match vecargs with
        | Types.listVal v =>
          evalFuncVal ref fn (firstargs ++ v) false
        | Types.vecVal v =>
          evalFuncVal ref fn (firstargs ++ (toList v)) false
        | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")

  partial def evalFnNative (env : Env) (name: String) (results: List Types) (args: List Types): IO (Env × Types) := do
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
      | "swap!" => swapAtom ref results args
      | "prn" => prnFunc ref results
      | "pr-str" => prStrFunc ref results
      | "str" => strFunc ref results
      | "println" => printlnFunc ref results
      | "eval" => eval ref results
      | "read-string" => match readString results ref with -- readString results Dict.empty
        | Except.error e => throw (IO.userError e)
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
              | "vector?" => Except.ok (ref, Types.boolVal true)
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
              | "fn?" => match func with
                | Fun.builtin _ => Except.ok (ref, Types.boolVal true)
                | Fun.userDefined _ _ _ => Except.ok (ref, Types.boolVal true)
                | Fun.macroFn _ _ _ =>  Except.ok (ref, Types.boolVal false)
              | "macro?" => match func with
                | Fun.builtin _ => Except.ok (ref, Types.boolVal false)
                | Fun.userDefined _ _ _ => Except.ok (ref, Types.boolVal false)
                | Fun.macroFn _ _ _ =>  Except.ok (ref, Types.boolVal true)
              | _ => Except.ok (ref, Types.boolVal false)
          | _   => throw (IO.userError s!"'{name}' not found")

end

def READ (input : String): Except String Types :=
  read_str.{u} input

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (env: Env) (input : String): Env × String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes ref result with
    | Except.error (newref, e) => (newref, s!"Error: {e}")
    | Except.ok (newref, res) => (newref, PRINT res)
  | Except.error err => (ref, s!"Parsing failed: {err}")

def printLogs (env : Env) : IO Unit :=
  forM (getLogsInfo ref) (fun elem =>
    match elem with
    | Types.strVal log => IO.println log
    | x => IO.println (x.toString true)
  )

def loadMalFns (env: Env) (fndefs: List String): Env × String :=
  fndefs.foldl (fun (res : Env × String) fndef =>
    let (ref, msg) := res
    let (newref, newmsg) := rep.{u} ref fndef
    (newref, s!"{msg}¬{newmsg}")
  ) (ref, "")

def fnDefs: List String := [
    "(def! *host-language* \"Lean\")",
    "(def! not (fn* (a) (if a false true)))",
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
  ]

def main (args : List String) : IO Unit := do
  let (env0, _) := loadMalFns.{u} (loadFnNativeAll (Env.data 0 Dict.empty)) fnDefs
  let astArgs := (args.map (fun arg => Types.strVal arg))
  let mut env := setSymbol env0 "*ARGV*" (Types.listVal astArgs)

  if args.length > 0 then
    let (ref, val) := rep.{u} env s!"(load-file \"{args[0]!}\")"
    printLogs ref
    IO.println val
  else

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
