import LeanMal.reader
import LeanMal.printer
import LeanMal.types

universe u

def READ (input : String): Except String Types :=
  read_str.{u} input

def sum (ref : Env) (lst: List Types) : Except String (Env × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x + y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x + y))
  | _                                    => Except.error "+ operator not supported"

def sub (ref : Env) (lst: List Types) : Except String (Env × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x - y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x - y))
  | _                                    => Except.error "- operator not supported"

def mul (ref : Env) (lst: List Types) : Except String (Env × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x * y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x * y))
  | _                                    => Except.error "* operator not supported"

def div (ref : Env) (lst: List Types) : Except String (Env × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x / y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x / y))
  | _                                    => Except.error "/ operator not supported"

def evalFnNative (ref : Env) (name: String) (results: List Types): Except String (Env × Types) :=
    match name with
    | "+" => sum ref results
    | "-" => sub ref results
    | "*" => mul ref results
    | "/" => div ref results
    | _   => Except.error s!"'{name}' not found"

mutual

  partial def evalTypes (ref : Env) (ast : Types) : Except String (Env × Types) :=
    match ast with
    | Types.symbolVal v   => match ref.get (KeyType.strKey v) with
      | some (_, vi) => Except.ok (ref, vi)
      | none => Except.error s!"'{v}' not found"
    | Types.listVal el    => (evalList ref el)
    | Types.vecVal el     => (evalVec ref (toList el))
    | Types.dictVal el    => (evalDict ref el)
    | x                   => Except.ok (ref, x)

  partial def evalFunc (ref: Env) (head : Types) (args : List Types) : Except String (Env × Types) :=
    match evalTypes ref head with
    | Except.error e => Except.error s!"error evaluating function: {head.toString true}: {e}"
    | Except.ok (ref2, fn) => evalFuncVal ref2 fn args

  partial def evalFuncVal (ref: Env) (fn: Types) (args: List Types) : Except String (Env × Types) :=
    -- first execute each function argument - reduce computation
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) =>
      match fn with
        | Types.funcVal v      => match v with
          | Fun.builtin name => evalFnNative newRef name results
          | Fun.userDefined fref params body =>
            let keys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let argsLevel := fref.getLevel + 1
            let argsDict := (buildDict argsLevel keys results)
            let merged := (newRef.merge fref).mergeDict argsLevel argsDict
            evalTypes merged body
          | Fun.macroFn _ _ _ => Except.error "macro not implemented"
        | _ => Except.error s!"`unexpected token, expected: function`"

  partial def evalList (ref: Env) (lst : List Types) : Except String (Env × Types) :=
    if List.length lst == 0 then Except.ok (ref, Types.listVal lst)
    else
      let head := lst[0]!
      match lst[0]! with
      | Types.symbolVal v => match v with
        | "def!" => evalDefn ref (lst.drop 1)
        | "let*" => evalLet ref (lst.drop 1)
        | _ => evalFunc ref head (lst.drop 1)
      | _ => evalFunc ref head (lst.drop 1)

  partial def evalVec (ref: Env) (elems : List Types) : Except String (Env × Types) :=
    match evalFuncArgs ref elems with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) => Except.ok (newRef, Types.vecVal (listToVec results))

  partial def evalDict (ref: Env) (lst : Dict) : Except String (Env × Types) :=
    match evalDictInner ref lst with
      | Except.error e => Except.error e
      | Except.ok (newRef, newDict) => Except.ok (newRef, Types.dictVal newDict)

  partial def evalDictInner (ref: Env) (lst : Dict) : Except String (Env × Dict) :=
    match lst with
      | Dict.empty => Except.ok (ref, lst)
      | Dict.insert k _ v restDict => match evalTypes ref v with
        | Except.error e => Except.error e
        | Except.ok (newRef, newVal) => match evalDictInner newRef restDict with
          | Except.error e => Except.error e
          | Except.ok (updatedRef, updatedDict) =>
            let newDict := Dict.insert k 0 newVal updatedDict
            Except.ok (updatedRef, newDict)

  partial def evalFuncArgs (ref: Env) (args: List Types) : Except String (Env × List Types) :=
    match args.foldl (fun (res : Except String (Env × List Types)) x =>
        match res with
        | Except.error e => Except.error s!"error evaluating function argument accumulator: {x.toString true}: {e}"
        | Except.ok (r, acc) => match evalTypes r x with
          | Except.error e => Except.error s!"error evaluating function argument: {x.toString true}: {e}"
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, acc ++ [res])
      ) (Except.ok (ref, [])) with
      | Except.error e => Except.error e
      | Except.ok (newRef, results) => Except.ok (newRef, results)

  partial def evalDefn (ref: Env) (args : List Types) : Except String (Env × Types) :=
    if args.length < 2 then Except.error "def! unexpected syntax"
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error e => Except.error s!"def!: {e}"
      | Except.ok (newRef, value) =>
        match key with
        | Types.symbolVal v =>
          let refResult := newRef.add (KeyType.strKey v) ref.getLevel value
          Except.ok (refResult, value)
        | _ => Except.error s!"def! unexpected token, expected: symbol"

  partial def evalLet (ref: Env) (args : List Types) : Except String (Env × Types) :=
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
      | Except.ok newRef => match evalTypes newRef body with
        | Except.error e => Except.error e
        -- we do not propagate the let* environment to the parent scope
        | Except.ok (_, result) => Except.ok (ref, result)

  partial def evalLetArgs (ref: Env) (args : List Types) : Except String Env :=
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

def loadFnNativeAll (ref: Env) : Env :=
  loadFnNative (
    loadFnNative (
      loadFnNative (
        loadFnNative ref "+"
      ) "-"
    ) "*"
  ) "/"

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (ref: Env) (input : String): Env × String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes ref result with
    | Except.error e => (ref, e)
    | Except.ok (newref, res) => (newref, PRINT res)
  | Except.error err => (ref, s!"Parsing failed: {err}")

def main : IO Unit := do
  IO.println "Welcome to Mal REPL!"
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
