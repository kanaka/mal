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
    let newfn := Fun.userDefined env.increment params body
    return (env, Types.funcVal newfn)

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
  partial def evalTypes (env : Env) (ast : Types) : IO (Env × Types) := do
    if getDebugEval env then IO.println s!"EVAL:{pr_str true ast}"
    match ast with
    | Types.symbolVal v   => match env.get (KeyType.strKey v) with
      | some (_, vi) => return (env, vi)
      | none => throw (IO.userError s!"'{v}' not found")
    | Types.listVal el    => (evalList env el)
    | Types.vecVal el     => (evalVec env (toList el))
    | Types.dictVal el    => (evalDict env el)
    | x                   => return (env, x)

  partial def evalFunc (env: Env) (head : Types) (args : List Types) : IO (Env × Types) := do
    let (env2, fn) ← evalTypes env head
    let (fref, res) ← evalFuncVal env2 fn args true
    return ((forwardOuterScopeDefs fref env), res)

  partial def evalFuncVal (env: Env) (fn: Types) (args: List Types) (evaluateArgs: Bool) : IO (Env × Types) := do
    match fn with
      | Types.funcVal v    => match v with
        | Fun.builtin name =>
          let (newEnv, results) ← if !evaluateArgs then
            pure (env, args)
          else
            evalFuncArgs env args
          evalFnNative newEnv name results args
        | Fun.userDefined fenv params body =>
          let (newEnv, results) ← if !evaluateArgs then
            pure (env, args)
          else
            evalFuncArgs env args

          let allkeys: List String := match params with
            | Types.listVal v => v.map fun x => x.toString false
            | _               => []
          let (keys, variadic) := splitOnAmpersand allkeys
          let normalArgs := results.take keys.length
          let variadicArg := results.drop keys.length
          let argVals := normalArgs ++ [Types.listVal variadicArg]
          let argsLevel := if fenv.getLevel >= newEnv.getLevel then fenv.getLevel + 1 else newEnv.getLevel + 1
          let argsDict := (buildDict argsLevel (keys ++ variadic) argVals)
          let merged := (newEnv.merge fenv).mergeDict argsLevel argsDict
          evalTypes merged body
        | Fun.macroFn fenv params body =>
          let allkeys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let (keys, variadic) := splitOnAmpersand allkeys
          let normalArgs := args.take keys.length
          let variadicArg := args.drop keys.length
          let argVals := normalArgs ++ [Types.listVal variadicArg]
          let argsLevel := if fenv.getLevel >= env.getLevel then fenv.getLevel + 1 else env.getLevel + 1
          let argsDict := (buildDict argsLevel (keys ++ variadic) argVals)
          let merged := (env.merge fenv).mergeDict argsLevel argsDict
          let (_, newast) ← evalTypes merged body
          evalTypes env newast
      | _ => throw (IO.userError s!"`unexpected token, expected: function`")

  partial def evalList (env: Env) (lst : List Types) : IO (Env × Types) := do
    if List.length lst == 0 then return (env, Types.listVal lst)
    else
      let head := lst[0]!
      match head with
      | Types.symbolVal v => match v with
        | "def!" => evalDefn env (lst.drop 1)
        | "let*" => evalLet env (lst.drop 1)
        | "do" => evalDo env (lst.drop 1)
        | "if" => evalIf env (lst.drop 1)
        | "fn*" => makeFn env (lst.drop 1)
        | "try*" => evalTry env (lst.drop 1)
        | "quote" =>
          if lst.length < 2 then throw (IO.userError "quote: expected 1 argument")
          else return (env, lst[1]!)
        | "quasiquote" =>
          if lst.length < 2 then throw (IO.userError "quasiquote: expected 1 argument")
          else evalTypes env (quasiquote lst[1]!)
        | "defmacro!" => evalDefMacro env (lst.drop 1)
        | _ => evalFunc env head (lst.drop 1)
      | _ => evalFunc env head (lst.drop 1)

  partial def evalVec (env: Env) (elems : List Types) : IO (Env × Types) := do
    let (newEnv, results) ← evalFuncArgs env elems
    return (newEnv, Types.vecVal (listToVec results))

  partial def evalDict (env: Env) (lst : Dict) : IO (Env × Types) := do
    let (newEnv, newDict) ← evalDictInner env lst
    return (newEnv, Types.dictVal newDict)

  partial def evalDictInner (env: Env) (lst : Dict) : IO (Env × Dict) := do
    match lst with
      | Dict.empty => return (env, lst)
      | Dict.insert k _ v restDict =>
        let (newEnv, newVal) ← evalTypes env v
        let (updatedEnv, updatedDict) ← evalDictInner newEnv restDict
        let newDict := Dict.insert k 0 newVal updatedDict
        return (updatedEnv, newDict)

  partial def evalFuncArgs (env: Env) (args: List Types) : IO (Env × List Types) :=
    args.foldlM (fun (res : Env × List Types) (x : Types) => do
      let (r, acc) := res
      let (updatedenv, res) ← evalTypes r x
      return (updatedenv, acc ++ [res])
    ) (env, [])

  partial def evalDefn (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "def! unexpected syntax")
    else
      let key := args[0]!
      let body := args[1]!
      let (newEnv, value) ← (evalTypes env body)
      match key with
        | Types.symbolVal v =>
          let envResult := newEnv.add (KeyType.strKey v) env.getLevel value
          return (envResult, value)
        | _ => throw (IO.userError s!"def! unexpected token, expected: symbol")

  partial def evalDefMacro (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "def! unexpected syntax")
    else
      let key := args[0]!
      let body := args[1]!
      let (newEnv, value) ← evalTypes env body
      match key with
      | Types.symbolVal v =>
        match value with
        | Types.funcVal func =>
          match func with
          | Fun.macroFn _ _ _ =>
            let envResult := newEnv.add (KeyType.strKey v) env.getLevel value
            return (envResult, value)
          | Fun.userDefined fenv params body =>
            let envResult := newEnv.add (KeyType.strKey v) env.getLevel (Types.funcVal (Fun.macroFn fenv params body))
            return (envResult, value)
          | _ => throw (IO.userError s!"defmacro!: unexpected builtin function")
        | x => throw (IO.userError s!"unexpected token type: {x.toString true}, expected: function")
      | _ => throw (IO.userError s!"def! unexpected token, expected: symbol")

  partial def evalLet (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "let*: unexpected syntax")
    else
      let pairs := args[0]!
      let body := args[1]!
      let newEnv ← match pairs with
      | Types.listVal v => evalLetArgs env.increment v
      | Types.vecVal v => evalLetArgs env.increment (toList v)
      | _ => throw (IO.userError s!"unexpected token type: ${pairs.toString true}, expected: list or vector")

      let (letenv, result) ← evalTypes newEnv body
      return ((forwardOuterScopeDefs letenv env), result)

  partial def evalLetArgs (env: Env) (args : List Types) : IO Env := do
    match args with
    | [] => return env
    | [_] => throw (IO.userError "let*: unexpected syntax")
    | x :: y :: rest =>
      match x with
      | Types.symbolVal key =>
        let (updatedEnv, value) ← evalTypes env y
        evalLetArgs (updatedEnv.add (KeyType.strKey key) env.getLevel value) rest
      | _ => throw (IO.userError "let*: unexpected syntax")

  partial def evalDo (env: Env) (args : List Types) : IO (Env × Types) := do
    -- only return last computation result
    let (newEnv, results) ← evalFuncArgs env args
    if results.length == 0 then return (newEnv, Types.Nil)
    else return (newEnv, results[results.length - 1]!)

  partial def evalIf (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "unexpected syntax")
    else
      let condition := args[0]!
      let thenExpr := args[1]!
      let hasElse := args.length > 2

      let (newEnv, condResp) ← evalTypes env condition
      let cond := match condResp with
      | Types.boolVal v => v
      | Types.Nil => false
      | _ => true
      if cond then evalTypes newEnv thenExpr
      else if hasElse then evalTypes newEnv args[2]!
      else return (newEnv, Types.Nil)

  partial def evalTry (env: Env) (lst : List Types) : IO (Env × Types) := do
    if lst.length < 1 then throw (IO.userError "try*: unexpected syntax")
    else
      try
        let (newEnv, result) ← evalTypes env lst[0]!
        return (newEnv, result)
      catch
      | evalErr =>
        if lst.length < 2 then throw evalErr
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
                      let err := Types.strVal evalErr.toString
                      if catchBody.length < 2 then throw (IO.userError "try*: unexpected syntax")
                      else
                        let toeval := catchBody[2]!
                        let built := buildDictWithSymbols env.getDict env.getLevel [errorSymbol] [err]
                        let merged := env.mergeDict (env.getLevel + 1) built
                        evalTypes merged toeval
                    | _ => throw (IO.userError s!"unexpected return type, expected: symbol")
                else throw evalErr
              | _ => throw evalErr
          -- | Types.vecVal v => -- TODO
          | _ => throw evalErr

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
        match env.get (KeyType.strKey sym) with
        | none => throw (IO.userError s!"{sym} not found")
        | some (level, _) => match first with
          | Types.atomVal x => match x with
            | Atom.v v =>
              let (_, res) ← evalFuncVal env fn ([v] ++ rest) false
              let newEnv := env.add (KeyType.strKey sym) level (Types.atomVal (Atom.v res))
              return (newEnv, res)
            | Atom.withmeta v meta =>
              let (_, res) ← evalFuncVal env fn ([v] ++ rest) false
              let newEnv := env.add (KeyType.strKey sym) level (Types.atomVal (Atom.withmeta res meta))
              return (newEnv, res)
          | x => throw (IO.userError s!"swap!: unexpected symbol: {x.toString true}, expected: atom")
      | x => throw (IO.userError s!"swap!: unexpected symbol: {x.toString true}, expected: function")
    | x => throw (IO.userError s!"swap!: unexpected token: {x.toString true}, expected: symbol")

  partial def eval (env: Env) (lst : List Types) : IO (Env × Types) := do
    if lst.length < 1 then throw (IO.userError "eval: unexpected syntax")
    else
      let ast := lst[0]!
      -- any new variables are defined on level 0
      let env0 := Env.data 0 env.getDict
      evalTypes env0 ast

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
  let finalResult ← args.foldlM (fun (res : (Env × List Types)) (x : Types) => do
    let (r, acc) := res
    let (updatedRef, res) ← evalFuncVal r fn [x] false
    pure (updatedRef, acc ++ [res])
  ) (env, [])

  let (newEnv, results) := finalResult
  pure (newEnv, Types.listVal results)

  partial def nativeMap (env: Env) (lst: List Types) : IO (Env × Types) := do
    if lst.length < 2 then throw (IO.userError "map: unexpected syntax")
    else
      let fn := lst[0]!
      let params := lst[1]!
      match fn with
      | Types.funcVal _ =>
        match params with
        | Types.listVal v => nativeMapOverList env fn v
        | Types.vecVal v => nativeMapOverList env fn (toList v)
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
          evalFuncVal env fn (firstargs ++ v) false
        | Types.vecVal v =>
          evalFuncVal env fn (firstargs ++ (toList v)) false
        | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")

  partial def evalFnNative (env : Env) (name: String) (results: List Types) (args: List Types): IO (Env × Types) := do
      match name with
      | "+" => sum env results
      | "-" => sub env results
      | "*" => mul env results
      | "/" => div env results
      | "<" => lt env results
      | "<=" => lte env results
      | ">" => gt env results
      | ">=" => gte env results
      | "=" => eq env results false
      | "list" => return (env, Types.listVal results)
      | "count" => countFunc env results
      | "cons" => cons env results
      | "concat" => concat env results
      | "map" => nativeMap env results
      | "apply" => nativeApply env results
      | "vec" => makeVec env results
      | "vector" => makeVector env results
      | "nth" => nthSeq env results
      | "first" => firstSeq env results
      | "rest" => restSeq env results
      | "conj" => conj env results
      | "seq" => seq env results
      | "hash-map" => makeDict env results
      | "assoc" => assocDict env results
      | "dissoc" => dissocDict env results
      | "get" => getDict env results
      | "contains?" => containsDict env results
      | "keys" => getKeysDict env results
      | "vals" => getValuesDict env results
      | "throw" => throwFn env results
      | "symbol" => makeSymbol env results
      | "keyword" => makeKeyword env results
      | "atom" => makeAtom env results
      | "deref" => derefAtom env results
      | "reset!" => resetAtom env results args
      | "swap!" => swapAtom env results args
      | "prn" => prnFunc env results
      | "pr-str" => prStrFunc env results
      | "str" => strFunc env results
      | "println" => printlnFunc env results
      | "eval" => eval env results
      | "read-string" =>
      let res ← readString results env -- readString results Dict.empty
      return (env, res)
      | _ => match results with
          | [x] => match x with
            | Types.Nil => if name == "nil?" then return (env, Types.boolVal true) else return (env, Types.boolVal false)
            | Types.intVal _ => if name == "number?" then return (env, Types.boolVal true) else return (env, Types.boolVal false)
            | Types.floatVal _ => if name == "number?" then return (env, Types.boolVal true) else return (env, Types.boolVal false)
            | Types.strVal _ => if name == "string?" then return (env, Types.boolVal true) else return (env, Types.boolVal false)
            | Types.symbolVal _ => if name == "symbol?" then return (env, Types.boolVal true) else return (env, Types.boolVal false)
            | Types.keywordVal _ => if name == "keyword?" then return (env, Types.boolVal true) else return (env, Types.boolVal false)
            | Types.dictVal _ => if name == "map?" then return (env, Types.boolVal true) else return (env, Types.boolVal false)
            | Types.listVal x => match name with
              | "list?" => return (env, Types.boolVal true)
              | "sequential?" => return (env, Types.boolVal true)
              | "empty?" => return (env, Types.boolVal (x.length == 0))
              | _ => return (env, Types.boolVal false)
            | Types.vecVal x => match name with
              | "sequential?" => return (env, Types.boolVal true)
              | "vector?" => return (env, Types.boolVal true)
              | "empty?" => return (env, Types.boolVal ((toList x).length == 0))
              | _ => return (env, Types.boolVal false)
            | Types.boolVal x => match name with
              | "true?" => return (env, Types.boolVal x)
              | "false?" => return (env, Types.boolVal !x)
              | _ => return (env, Types.boolVal false)
            | Types.atomVal _ => match name with
              | "atom?" => return (env, Types.boolVal true)
              | _ => return (env, Types.boolVal false)
            | Types.funcVal func => match name with
              | "fn?" => match func with
                | Fun.builtin _ => return (env, Types.boolVal true)
                | Fun.userDefined _ _ _ => return (env, Types.boolVal true)
                | Fun.macroFn _ _ _ =>  return (env, Types.boolVal false)
              | "macro?" => match func with
                | Fun.builtin _ => return (env, Types.boolVal false)
                | Fun.userDefined _ _ _ => return (env, Types.boolVal false)
                | Fun.macroFn _ _ _ =>  return (env, Types.boolVal true)
              | _ => return (env, Types.boolVal false)
          | _   => throw (IO.userError s!"'{name}' not found")

end

def READ (input : String): Except String Types :=
  read_str input

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (env: Env) (input : String): IO (Env × String) := do
  match READ input with
  | Except.ok result =>
    try
      let (newenv, res) ← evalTypes env result
      return (newenv, PRINT res)
    catch
      | e => return (env, s!"Error: {e}")
  | Except.error err => return (env, s!"Parsing failed: {err}")

def loadMalFns (env: Env) (fndefs: List String): IO (Env × String) := do
  fndefs.foldlM (fun (res : Env × String) fndef => do
    let (ref, msg) := res
    let (newref, newmsg) ← rep ref fndef
    return (newref, s!"{msg}¬{newmsg}")
  ) (env, "")

def fnDefs: List String := [
    "(def! *host-language* \"Lean\")",
    "(def! not (fn* (a) (if a false true)))",
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))",
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
  ]

def repAndPrint (env: Env) (output : String): IO Env := do
  if output.endsWith "endofinput" then IO.print ""
  else IO.println output
  return env

def reploop (inienv: Env) : IO Unit := do
  let mut donext := false
  let mut env := inienv
  donext := true
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
      let (newenv, value) ← rep env value
      env ← repAndPrint newenv value

def main (args : List String) : IO Unit := do
  let (env0, _) ← loadMalFns (loadFnNativeAll (Env.data 0 Dict.empty)) fnDefs
  let env := setSymbol env0 "*ARGV*" (Types.listVal [])

  if args.length > 0 then do
    let astArgs := ((args.drop 1).map (fun arg => Types.strVal arg))
    let newenv := setSymbol env0 "*ARGV*" (Types.listVal astArgs)
    let (_, _) ← rep newenv s!"(load-file \"{args[0]!}\")"
    IO.Process.exit 0
  else reploop env
