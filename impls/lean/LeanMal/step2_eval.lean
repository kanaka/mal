import LeanMal.reader
import LeanMal.printer

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
      | none    => Except.ok (ref, Types.symbolVal v )
    | Types.listVal el    => (evalList ref el)
    | Types.vecVal el     => (evalVec ref (toList el))
    | Types.dictVal el    => (evalDict ref el)
    | x                   => Except.ok (ref, x)

  partial def evalFunc (ref: Env) (head : Types) (args : List Types) : Except String (Env × Types) :=
    match evalTypes ref head with
    | Except.error e => Except.error s!"error evaluating function: {head.toString true}: {e}"
    | Except.ok (_, fn) => evalFuncVal ref fn args

  partial def evalFuncVal (ref: Env) (fn: Types) (args: List Types) : Except String (Env × Types) :=
    -- first execute each function argument - reduce computation
    match evalFuncArgs ref args with
    | Except.error e => Except.error e
    | Except.ok (newRef, results) =>
      match fn with
        | Types.symbolVal name => evalFnNative newRef name results
        | Types.funcVal v      => match v with
          | Fun.builtin name => evalFnNative newRef name results
          | Fun.userDefined fref params body =>
            let keys: List String := match params with
              | Types.listVal v => v.map fun x => x.toString false
              | _               => []
            let argsDict := (buildDict 0 keys results)
            let merged := (newRef.merge fref).mergeDict (fref.getLevel + 1) argsDict
            evalTypes merged body
          | Fun.macroFn _ _ _ => Except.error "macro not implemented"
        | _ => Except.error s!"`unexpected token, expected: function`"

  partial def evalList (ref: Env) (lst : List Types) : Except String (Env × Types) :=
    if List.length lst == 0 then Except.ok (ref, Types.listVal lst)
    else
      let head := lst[0]!
      match lst[0]! with
      | Types.symbolVal v => match v with
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
end

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (input : String): String :=
  match READ.{u} input with
  | Except.ok result => match evalTypes (Env.data 0 Dict.empty) result with
    | Except.error e => e
    | Except.ok (_, res) => PRINT res
  | Except.error err =>
    s!"Parsing failed: {err}"

def main : IO Unit := do
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
      IO.println (rep.{u} value)
