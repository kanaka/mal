import LeanMal.reader
import LeanMal.printer
import LeanMal.types

universe u

def READ (input : String): Except String Types :=
  read_str.{u} input

def sum (ref : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (ref, Types.intVal 0)
  | [Types.intVal x]                     => return (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (ref, Types.intVal (x + y))
  | [Types.floatVal x]                   => return (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (ref, Types.floatVal (x + y))
  | _                                    => throw (IO.userError "+ operator not supported")

def sub (ref : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (ref, Types.intVal 0)
  | [Types.intVal x]                     => return (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (ref, Types.intVal (x - y))
  | [Types.floatVal x]                   => return (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (ref, Types.floatVal (x - y))
  | _                                    => throw (IO.userError "- operator not supported")

def mul (ref : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (ref, Types.intVal 0)
  | [Types.intVal x]                     => return (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (ref, Types.intVal (x * y))
  | [Types.floatVal x]                   => return (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (ref, Types.floatVal (x * y))
  | _                                    => throw (IO.userError "* operator not supported")

def div (ref : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (ref, Types.intVal 0)
  | [Types.intVal x]                     => return (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (ref, Types.intVal (x / y))
  | [Types.floatVal x]                   => return (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (ref, Types.floatVal (x / y))
  | _                                    => throw (IO.userError "/ operator not supported")

def evalFnNative (ref : Env) (name: String) (results: List Types): IO (Env × Types) := do
    match name with
    | "+" => sum ref results
    | "-" => sub ref results
    | "*" => mul ref results
    | "/" => div ref results
    | _   => throw (IO.userError s!"'{name}' not found")

mutual

  partial def evalTypes (ref : Env) (ast : Types) : IO (Env × Types) := do
    match ast with
    | Types.symbolVal v   => match ref.get (KeyType.strKey v) with
      | some (_, vi) => return (ref, vi)
      | none => throw (IO.userError s!"'{v}' not found")
    | Types.listVal el    => (evalList ref el)
    | Types.vecVal el     => (evalVec ref (toList el))
    | Types.dictVal el    => (evalDict ref el)
    | x                   => return (ref, x)

  partial def evalFunc (env: Env) (head : Types) (args : List Types) : IO (Env × Types) := do
    match evalTypes ref head with
    | Except.error e => Except.error s!"error evaluating function: {head.toString true}: {e}"
    | Except.ok (_, fn) => evalFuncVal ref fn args

  partial def evalFuncVal (env: Env) (fn: Types) (args: List Types) : IO (Env × Types) := do
    -- first execute each function argument - reduce computation
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newEnv, results) =>
      match fn with
        | Types.funcVal v      => match v with
          | Fun.builtin name => evalFnNative newEnv name results
          | Fun.userDefined fref params body =>
            let keys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let argsLevel := fref.getLevel + 1
            let argsDict := (buildDict argsLevel keys results)
            let merged := (newEnv.merge fref).mergeDict argsLevel argsDict
            evalTypes merged body
          | Fun.macroFn _ _ _ => Except.error "macro not implemented"
        | _ => Except.error s!"`unexpected token, expected: function`"

  partial def evalList (env: Env) (lst : List Types) : IO (Env × Types) := do
    if List.length lst == 0 then Except.ok (ref, Types.listVal lst)
    else
      let head := lst[0]!
      match lst[0]! with
      | Types.symbolVal v => match v with
        | "def!" => evalDefn ref (lst.drop 1)
        | "let*" => evalLet ref (lst.drop 1)
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

  partial def evalDictInner (env: Env) (lst : Dict) : IO (Env × Types) := do
    match lst with
      | Dict.empty => Except.ok (ref, lst)
      | Dict.insert k _ v restDict => match evalTypes ref v with
        | Except.error e => Except.error e
        | Except.ok (newEnv, newVal) => match evalDictInner newEnv restDict with
          | Except.error e => Except.error e
          | Except.ok (updatedRef, updatedDict) =>
            let newDict := Dict.insert k 0 newVal updatedDict
            Except.ok (updatedRef, newDict)

  partial def evalFuncArgs (env: Env) (args: List Types) : IO (Env × List Types) := do
    match args.foldl (fun (res : IO (Dict × List Types)) x =>
        match res with
        | Except.error e => Except.error s!"error evaluating function argument accumulator: {x.toString true}: {e}"
        | Except.ok (r, acc) => match evalTypes r x with
          | Except.error e => Except.error s!"error evaluating function argument: {x.toString true}: {e}"
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, acc ++ [res])
      ) (Except.ok (ref, [])) with
      | Except.error e => Except.error e
      | Except.ok (newEnv, results) => Except.ok (newEnv, results)

  partial def evalDefn (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then Except.error "def! unexpected syntax"
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error e => Except.error s!"def!: {e}"
      | Except.ok (newEnv, value) =>
        match key with
        | Types.symbolVal v =>
          let refResult := newEnv.add (KeyType.strKey v) ref.getLevel value
          Except.ok (refResult, value)
        | _ => Except.error s!"def! unexpected token, expected: symbol"

  partial def evalLet (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then Except.error "let*: unexpected syntax"
    else
      let pairs := args[0]!
      let body := args[1]!
      let result := match pairs with
      | Types.listVal v => evalLetArgs ref.increment v
      | Types.vecVal v => evalLetArgs ref.increment (toList v)
      | _ => Except.error s!"unexpected token type: ${pairs.toString true}, expected: list or vector"

      match result with
      | Except.error e => Except.error s!"let*: {e}"
      | Except.ok newEnv => match evalTypes newEnv body with
        | Except.error e => Except.error e
        -- we do not propagate the let* environment to the parent scope
        | Except.ok (_, result) => Except.ok (ref, result)

  partial def evalLetArgs (env: Env) (args : List Types) : IO Env :=
    match args with
    | [] => Except.ok ref
    | [_] => Except.error "let*: unexpected syntax"
    | x :: y :: rest =>
      match x with
      | Types.symbolVal key => match evalTypes ref y with
        | Except.error e => Except.error s!"error evaluating function argument: {key}: {e}"
        | Except.ok (updatedRef, value) =>
          evalLetArgs (updatedRef.add (KeyType.strKey key) ref.getLevel value) rest
      | _ => Except.error "let*: unexpected syntax"
end

def loadFnNative (ref : Env) (name: String) : Env :=
  ref.add (KeyType.strKey name) 0 (Types.funcVal (Fun.builtin name))

def loadFnNativeAll (env: Env) : Env :=
  loadFnNative (
    loadFnNative (
      loadFnNative (
        loadFnNative ref "+"
      ) "-"
    ) "*"
  ) "/"

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (env: Env) (input : String): Env × String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes ref result with
    | Except.error e => (ref, e)
    | Except.ok (newref, res) => (newref, PRINT res)
  | Except.error err => (ref, s!"Parsing failed: {err}")

def main : IO Unit := do
  let mut env := loadFnNativeAll (Env.data 0 Dict.empty)
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
      IO.println val
      env := ref
