import LeanMal.reader
import LeanMal.printer

universe u

def READ (input : String): Except String Types :=
  read_str.{u} input

def sum (ref : Dict := Dict.empty) (lst: List Types) : Except String (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x + y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x + y))
  | _                                    => Except.error "+ operator not supported"

def sub (ref : Dict := Dict.empty) (lst: List Types) : Except String (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x - y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x - y))
  | _                                    => Except.error "- operator not supported"

def mul (ref : Dict := Dict.empty) (lst: List Types) : Except String (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x * y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x * y))
  | _                                    => Except.error "* operator not supported"

def div (ref : Dict := Dict.empty) (lst: List Types) : Except String (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x / y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x / y))
  | _                                    => Except.error "/ operator not supported"

def evalFnNative (ref : Dict := Dict.empty) (name: String) (results: List Types): Except String (Dict × Types) :=
    match name with
    | "+" => sum ref results
    | "-" => sub ref results
    | "*" => mul ref results
    | "/" => div ref results
    | _   => Except.error s!"'{name}' not found"

mutual

  partial def evalTypes (ref : Dict := Dict.empty) (ast : Types) : Except String (Dict × Types) :=
    match ast with
    | Types.symbolVal v   => match getEntry ref (KeyType.strKey v) with
      | some vi => Except.ok (ref, vi)
      | none => Except.error s!"'{v}' not found"
    | Types.listVal el    => (evalList ref el)
    | Types.vecVal el     => (evalVec ref (toList el))
    | Types.dictVal el    => (evalDict ref el)
    | x                   => Except.ok (ref, x)

  partial def evalFunc (ref: Dict) (head : Types) (args : List Types) : Except String (Dict × Types) :=
    match evalTypes ref head with
    | Except.error e => Except.error s!"error evaluating function: {head.toString true}: {e}"
    | Except.ok (ref2, fn) => evalFuncVal ref2 fn args

  partial def evalFuncVal (ref: Dict) (fn: Types) (args: List Types) : Except String (Dict × Types) :=
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

            let built := buildDictWithSymbols fref keys results
            let merged := mergeDicts newRef built
            evalTypes merged body
          | Fun.macroFn _ _ _ => Except.error "macro not implemented"
        | _ => Except.error s!"`unexpected token, expected: function`"

  partial def evalList (ref: Dict) (lst : List Types) : Except String (Dict × Types) :=
    if List.length lst == 0 then Except.ok (ref, Types.listVal lst)
    else
      let head := lst[0]!
      match lst[0]! with
      | Types.symbolVal v => match v with
        | "def!" => evalDefn ref (lst.drop 1)
        | "let*" => evalLet ref (lst.drop 1)
        | _ => evalFunc ref head (lst.drop 1)
      | _ => evalFunc ref head (lst.drop 1)

  partial def evalVec (ref: Dict) (elems : List Types) : Except String (Dict × Types) :=
    match evalFuncArgs ref elems with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) => Except.ok (newRef, Types.vecVal (listToVec results))

  partial def evalDict (ref: Dict) (lst : Dict) : Except String (Dict × Types) :=
    match evalDictInner ref lst with
      | Except.error e => Except.error e
      | Except.ok (newRef, newDict) => Except.ok (newRef, Types.dictVal newDict)

  partial def evalDictInner (ref: Dict) (lst : Dict) : Except String (Dict × Dict) :=
    match lst with
      | Dict.empty => Except.ok (ref, lst)
      | Dict.insert k v restDict => match evalTypes ref v with
        | Except.error e => Except.error e
        | Except.ok (newRef, newVal) => match evalDictInner newRef restDict with
          | Except.error e => Except.error e
          | Except.ok (updatedRef, updatedDict) =>
            let newDict := Dict.insert k newVal updatedDict
            Except.ok (updatedRef, newDict)

  partial def evalFuncArgs (ref: Dict) (args: List Types) : Except String (Dict × List Types) :=
    match args.foldl (fun (res : Except String (Dict × List Types)) x =>
        match res with
        | Except.error e => Except.error s!"error evaluating function argument accumulator: {x.toString true}: {e}"
        | Except.ok (r, acc) => match evalTypes r x with
          | Except.error e => Except.error s!"error evaluating function argument: {x.toString true}: {e}"
          | Except.ok (updatedRef, res) =>
            Except.ok (updatedRef, acc ++ [res])
      ) (Except.ok (ref, [])) with
      | Except.error e => Except.error e
      | Except.ok (newRef, results) => Except.ok (newRef, results)

  partial def evalDefn (ref: Dict) (args : List Types) : Except String (Dict × Types) :=
    if args.length < 2 then Except.error "def! unexpected syntax"
    else
      let key := args[0]!
      let body := args[1]!
      match (evalTypes ref body)  with
      | Except.error e => Except.error s!"def!: {e}"
      | Except.ok (newRef, value) =>
        match key with
        | Types.symbolVal v =>
          let refResult := addEntry newRef (KeyType.strKey v) value
          Except.ok (refResult, value)
        | _ => Except.error s!"def! unexpected token, expected: symbol"

  partial def evalLet (ref: Dict) (args : List Types) : Except String (Dict × Types) :=
    if args.length < 2 then Except.error "let*: unexpected syntax"
    else
      let pairs := args[0]!
      let body := args[1]!
      let result := match pairs with
      | Types.listVal v => evalLetArgs ref v
      | Types.vecVal v => evalLetArgs ref (toList v)
      | _ => Except.error s!"unexpected token type: ${pairs.toString true}, expected: list or vector"

      match result with
      | Except.error e => Except.error s!"let*: {e}"
      | Except.ok newRef => match evalTypes newRef body with
        | Except.error e => Except.error e
        -- we do not propagate the let* environment to the parent scope
        | Except.ok (_, result) => Except.ok (ref, result)

  partial def evalLetArgs (ref: Dict) (args : List Types) : Except String Dict :=
    match args with
    | [] => Except.ok ref
    | [_] => Except.error "let*: unexpected syntax"
    | x :: y :: rest =>
      match x with
      | Types.symbolVal key => match evalTypes ref y with
        | Except.error e => Except.error s!"error evaluating function argument: {key}: {e}"
        | Except.ok (updatedRef, value) =>
          evalLetArgs (addEntry updatedRef (KeyType.strKey key) value) rest
      | _ => Except.error "let*: unexpected syntax"
end

def loadFnNative (ref: Dict) (name: String) : Dict :=
  ref.insert (KeyType.strKey name) (Types.funcVal (Fun.builtin name))

def loadFnNativeAll (ref: Dict) : Dict :=
  loadFnNative (
    loadFnNative (
      loadFnNative (
        loadFnNative ref "+"
      ) "-"
    ) "*"
  ) "/"

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (ref: Dict) (input : String): Dict × String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes ref result with
    | Except.error e => (ref, e)
    | Except.ok (newref, res) => (newref, PRINT res)
  | Except.error err => (ref, s!"Parsing failed: {err}")

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
      IO.println val
      env := ref
