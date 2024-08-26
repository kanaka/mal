import LeanMal.reader
import LeanMal.printer
import LeanMal.core

universe u

def makeFn (ref: Env) (args : List Types) : IO (Env × Types) := do
  if args.length < 2 then Except.error (ref, "unexpected syntax")
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
      | none => Except.error (ref, s!"'{v}' not found")
    | Types.listVal el    => (evalList ref el)
    | Types.vecVal el     => (evalVec ref (toList el))
    | Types.dictVal el    => (evalDict ref el)
    | x                   => Except.ok (ref, x)

  partial def evalFunc (ref: Env) (head : Types) (args : List Types) : IO (Env × Types) := do
    match evalTypes ref head with
    | Except.error (newref, e) => Except.error (newref, s!"error evaluating function: {head.toString true}: {e}")
    | Except.ok (ref2, fn) =>
      match evalFuncVal ref2 fn args with
      | Except.error e => Except.error e
      | Except.ok (fref, res) =>
        -- after executing a function, propagate atoms (defined in outer environments) and logs to the parent scope
        Except.ok (forwardLogs fref (forwardMutatedAtoms fref ref), res)

  partial def evalFuncVal (ref: Env) (fn: Types) (args: List Types) : IO (Env × Types) := do
    -- first execute each function argument - reduce computation
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) =>
      match fn with
        | Types.funcVal v      => match v with
          | Fun.builtin name => evalFnNative newRef name results args
          | Fun.userDefined fref params body =>
            let allkeys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let (keys, variadic) := splitOnAmpersand allkeys
            let normalArgs := results.take keys.length
            let variadicArg := results.drop keys.length
            let argVals := normalArgs ++ [Types.listVal variadicArg]
            let argsLevel := if fref.getLevel >= newRef.getLevel then fref.getLevel + 1 else newRef.getLevel + 1

            let argsDict := (buildDict argsLevel (keys ++ variadic) argVals)
            let merged := (newRef.merge fref).mergeDict argsLevel argsDict

            evalTypes merged body
          | Fun.macroFn _ _ _ => Except.error (newRef, "macro not implemented")
        | _ => Except.error (newRef, s!"`unexpected token, expected: function`")

  partial def evalList (ref: Env) (lst : List Types) : IO (Env × Types) := do
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
        | _ => evalFunc ref head (lst.drop 1)
      | _ => evalFunc ref head (lst.drop 1)

  partial def evalVec (ref: Env) (elems : List Types) : IO (Env × Types) := do
    match evalFuncArgs ref elems with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) => Except.ok (newRef, Types.vecVal (listToVec results))

  partial def evalDict (ref: Env) (lst : Dict) : IO (Env × Types) := do
    match evalDictInner ref lst with
      | Except.error e => Except.error e
      | Except.ok (newRef, newDict) => Except.ok (newRef, Types.dictVal newDict)

  partial def evalDictInner (ref: Env) (lst : Dict) : Except (Env × String) (Env × Dict) :=
    match lst with
      | Dict.empty => Except.ok (ref, lst)
      | Dict.insert k _ v restDict => match evalTypes ref v with
        | Except.error e => Except.error e
        | Except.ok (newRef, newVal) => match evalDictInner newRef restDict with
          | Except.error e => Except.error e
          | Except.ok (updatedRef, updatedDict) =>
            let newDict := Dict.insert k 0 newVal updatedDict
            Except.ok (updatedRef, newDict)

  partial def evalFuncArgs (ref: Env) (args: List Types) : Except (Env × String) (Env × List Types) :=
    match args.foldl (fun (res : Except (Env × String) (Env × List Types)) x =>
        match res with
        | Except.error (newref, e) => Except.error (newref, s!"error evaluating function argument accumulator: {x.toString true}: {e}")
        | Except.ok (r, acc) => match evalTypes r x with
          | Except.error (newref, e) => Except.error (newref, s!"error evaluating function argument: {x.toString true}: {e}")
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, acc ++ [res])
      ) (Except.ok (ref, [])) with
      | Except.error e => Except.error e
      | Except.ok (newRef, results) => Except.ok (newRef, results)

  partial def evalDefn (ref: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then Except.error (ref, "def! unexpected syntax")
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error (newref, e) => Except.error (newref, s!"def!: {e}")
      | Except.ok (newRef, value) =>
        match key with
        | Types.symbolVal v =>
          let refResult := newRef.add (KeyType.strKey v) ref.getLevel value
          Except.ok (refResult, value)
        | _ => Except.error (newRef, s!"def! unexpected token, expected: symbol")

  partial def evalLet (ref: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then Except.error (ref, "let*: unexpected syntax")
    else
      let pairs := args[0]!
      let body := args[1]!
      let result := match pairs with
      | Types.listVal v => evalLetArgs ref.increment v
      | Types.vecVal v => evalLetArgs ref.increment (toList v)
      | _ => Except.error (ref, s!"unexpected token type: ${pairs.toString true}, expected: list or vector")

      match result with
      | Except.error (newRef, e) => Except.error (newRef, s!"let*: {e}")
      | Except.ok newRef => match evalTypes newRef body with
        | Except.error e => Except.error e
        -- after executing let*, propagate atoms (defined in outer environments) and logs to the parent scope
        | Except.ok (letref, result) =>
          Except.ok (forwardLogs letref (forwardMutatedAtoms letref ref), result)

  partial def evalLetArgs (ref: Env) (args : List Types) : Except (Env × String) Env :=
    match args with
    | [] => Except.ok ref
    | [_] => Except.error (ref, "let*: unexpected syntax")
    | x :: y :: rest =>
      match x with
      | Types.symbolVal key => match evalTypes ref y with
        | Except.error (newRef, e) => Except.error (newRef, s!"error evaluating function argument: {key}: {e}")
        | Except.ok (updatedRef, value) =>
          evalLetArgs (updatedRef.add (KeyType.strKey key) ref.getLevel value) rest
      | _ => Except.error (ref, "let*: unexpected syntax")

  partial def evalDo (ref: Env) (args : List Types) : IO (Env × Types) := do
    -- only return last computation result
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) =>
      if results.length == 0 then Except.ok (newRef, Types.Nil)
      else Except.ok (newRef, results[results.length - 1]!)

  partial def evalIf (ref: Env) (args : List Types) : IO (Env × Types) := do
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

  partial def swapAtom (ref: Env) (lst: List Types) (args: List Types) : IO (Env × Types) := do
  if lst.length < 2 then Except.error (ref, "swap!: >= 2 argument required")
  else
    let first := lst[0]!
    let fn := lst[1]!
    let rest := lst.drop 2
    match args[0]! with
    | Types.symbolVal sym =>
      match fn with
      | Types.funcVal _ =>
        match ref.get (KeyType.strKey sym) with
        | none => Except.error (ref, s!"{sym} not found")
        | some (level, _) => match first with
          | Types.atomVal x => match x with
            | Atom.v v =>
              match evalFuncVal ref fn ([v] ++ rest) with
              | Except.error (newRef, e) => Except.error (newRef, s!"swap! evaluate function: {e}")
              | Except.ok (_, res) =>
                let newRef := ref.add (KeyType.strKey sym) level (Types.atomVal (Atom.v res))
                Except.ok (newRef, res)
            | Atom.withmeta v meta =>
              match evalFuncVal ref fn ([v] ++ rest) with
              | Except.error (newRef, e) => Except.error (newRef, s!"swap! evaluate function: {e}")
              | Except.ok (_, res) =>
                let newRef := ref.add (KeyType.strKey sym) level (Types.atomVal (Atom.withmeta res meta))
                Except.ok (newRef, res)
          | x => Except.error (ref, s!"swap!: unexpected symbol: {x.toString true}, expected: atom")
      | x => Except.error (ref, s!"swap!: unexpected symbol: {x.toString true}, expected: function")
    | x => Except.error (ref, s!"swap!: unexpected token: {x.toString true}, expected: symbol")

  partial def eval (ref: Env) (lst : List Types) : IO (Env × Types) := do
    if lst.length < 1 then Except.error (ref, "eval: unexpected syntax")
    else
      let ast := lst[0]!
      evalTypes ref ast

  partial def evalFnNative (ref : Env) (name: String) (results: List Types) (args: List Types): IO (Env × Types) := do
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
          | Types.vecVal x => match name with
            | "empty?" => Except.ok (ref, Types.boolVal ((toList x).length == 0))
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

def rep (ref: Env) (input : String): Env × String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes ref result with
    | Except.error (newref, e) => (newref, e)
    | Except.ok (newref, res) => (newref, PRINT res)
  | Except.error err => (ref, s!"Parsing failed: {err}")

def printLogs (ref : Env) : IO Unit :=
  forM (getLogsInfo ref) (fun elem =>
    match elem with
    | Types.strVal log => IO.println log
    | x => IO.println (x.toString true)
  )

def loadMalFns (ref: Env) (fndefs: List String): Env × String :=
  fndefs.foldl (fun (res : Env × String) fndef =>
    let (ref, msg) := res
    let (newref, newmsg) := rep.{u} ref fndef
    (newref, s!"{msg}¬{newmsg}")
  ) (ref, "")

def fnDefs: List String := [
    "(def! not (fn* (a) (if a false true)))",
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
