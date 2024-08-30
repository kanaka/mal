import LeanMal.reader
import LeanMal.printer
import LeanMal.types

universe u

def READ (input : String): Except String Types :=
  read_str input

def sum (env : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (env, Types.intVal 0)
  | [Types.intVal x]                     => return (env, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (env, Types.intVal (x + y))
  | [Types.floatVal x]                   => return (env, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (env, Types.floatVal (x + y))
  | _                                    => throw (IO.userError "+ operator not supported")

def sub (env : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (env, Types.intVal 0)
  | [Types.intVal x]                     => return (env, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (env, Types.intVal (x - y))
  | [Types.floatVal x]                   => return (env, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (env, Types.floatVal (x - y))
  | _                                    => throw (IO.userError "- operator not supported")

def mul (env : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (env, Types.intVal 0)
  | [Types.intVal x]                     => return (env, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (env, Types.intVal (x * y))
  | [Types.floatVal x]                   => return (env, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (env, Types.floatVal (x * y))
  | _                                    => throw (IO.userError "* operator not supported")

def div (env : Env) (lst: List Types) : IO (Env × Types) := do
  match lst with
  | []                                   => return (env, Types.intVal 0)
  | [Types.intVal x]                     => return (env, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => return (env, Types.intVal (x / y))
  | [Types.floatVal x]                   => return (env, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => return (env, Types.floatVal (x / y))
  | _                                    => throw (IO.userError "/ operator not supported")

def evalFnNative (env : Env) (name: String) (results: List Types): IO (Env × Types) := do
    match name with
    | "+" => sum env results
    | "-" => sub env results
    | "*" => mul env results
    | "/" => div env results
    | _   => throw (IO.userError s!"'{name}' not found")

mutual

  partial def evalTypes (env : Env) (ast : Types) : IO (Env × Types) := do
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
    evalFuncVal env2 fn args

  partial def evalFuncVal (env: Env) (fn: Types) (args: List Types) : IO (Env × Types) := do
    -- first execute each function argument - reduce computation
    let (newEnv, results) ← evalFuncArgs env args
    match fn with
      | Types.funcVal v      => match v with
        | Fun.builtin name => evalFnNative newEnv name results
        | Fun.userDefined fenv params body =>
          let keys: List String := match params with
            | Types.listVal v => v.map fun x => x.toString false
            | _               => []
          let argsLevel := fenv.getLevel + 1
          let argsDict := (buildDict argsLevel keys results)
          let merged := (newEnv.merge fenv).mergeDict argsLevel argsDict
          evalTypes merged body
        | Fun.macroFn _ _ _ => throw (IO.userError "macro not implemented")
      | _ => throw (IO.userError s!"`unexpected token, expected: function`")

  partial def evalList (env: Env) (lst : List Types) : IO (Env × Types) := do
    if List.length lst == 0 then return (env, Types.listVal lst)
    else
      let head := lst[0]!
      match lst[0]! with
      | Types.symbolVal v => match v with
        | "def!" => evalDefn env (lst.drop 1)
        | "let*" => evalLet env (lst.drop 1)
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

  partial def evalFuncArgs (env: Env) (args: List Types) : IO (Env × List Types) := do
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

  partial def evalLet (env: Env) (args : List Types) : IO (Env × Types) := do
    if args.length < 2 then throw (IO.userError "let*: unexpected syntax")
    else
      let pairs := args[0]!
      let body := args[1]!
      let newEnv ← match pairs with
      | Types.listVal v => evalLetArgs env.increment v
      | Types.vecVal v => evalLetArgs env.increment (toList v)
      | _ => throw (IO.userError s!"unexpected token type: ${pairs.toString true}, expected: list or vector")

      -- we do not propagate the let* environment to the parent scope
      let (_, result) ← evalTypes newEnv body
      return (env, result)

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
end

def loadFnNative (env : Env) (name: String) : Env :=
  env.add (KeyType.strKey name) 0 (Types.funcVal (Fun.builtin name))

def loadFnNativeAll (env: Env) : Env :=
  loadFnNative (
    loadFnNative (
      loadFnNative (
        loadFnNative env "+"
      ) "-"
    ) "*"
  ) "/"

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

def repAndPrint (env: Env) (output : String): IO Env := do
  if output.endsWith "endofinput" then IO.print ""
  else IO.println output
  return env

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
      let (newenv, value) ← rep env value
      env ← repAndPrint newenv value
