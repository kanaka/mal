import Lean
import LeanMal.types
import LeanMal.reader

universe u

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

def ltInternal (first: Types) (second: Types) (orEq: Bool) : Bool :=
  match first, second with
  | Types.intVal n, Types.intVal v => n < v || (if orEq then n == v else false)
  | Types.intVal n, Types.floatVal v => (Float.ofInt n) < v || (if orEq then (Float.ofInt n) == v else false)
  | Types.floatVal n, Types.floatVal v => n < v || (if orEq then n == v else false)
  | Types.floatVal n, Types.intVal v => n < (Float.ofInt v) || (if orEq then n == (Float.ofInt v) else false)
  | Types.strVal n, Types.strVal v => n < v || (if orEq then n == v else false)
  | _, _ => false

def lt (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := ltInternal first second false
    return (env, Types.boolVal res)

def lte (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := ltInternal first second true
    return (env, Types.boolVal res)

def gtInternal (first: Types) (second: Types) (orEq: Bool) : Bool :=
  match first, second with
  | Types.intVal n, Types.intVal v => n > v || (if orEq then n == v else false)
  | Types.intVal n, Types.floatVal v => (Float.ofInt n) > v || (if orEq then (Float.ofInt n) == v else false)
  | Types.floatVal n, Types.floatVal v => n > v || (if orEq then n == v else false)
  | Types.floatVal n, Types.intVal v => n > (Float.ofInt v) || (if orEq then n == (Float.ofInt v) else false)
  | Types.strVal n, Types.strVal v => n > v || (if orEq then n == v else false)
  | _, _ => false

def gt (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := gtInternal first second false
    return (env, Types.boolVal res)

def gte (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := gtInternal first second true
    return (env, Types.boolVal res)

mutual
  partial def eqList (n: List Types) (v: List Types) (strict: Bool) : Bool :=
    match n, v with
    | [], [] => true
    | [], _ => false
    | _, [] => false
    | a :: nrest, b :: vrest =>
      if !(eqInternal a b strict) then false
      else eqList nrest vrest strict

  -- partial def eqDictKeys (k1: List KeyType) (k2: List KeyType) : Bool :=
  --   match k1, k2 with
  --   | KeyType.strKey x,

  partial def eqDict (n: Dict) (v: Dict) (strict: Bool) : Bool :=
    match n, v with
    | Dict.empty, Dict.empty => true
    | d1, d2 =>
      let keys1 := d1.keys
      let keys2 := d2.keys
      if keys1.length != keys2.length then false
      else
        keys1.all (fun k =>
          match (d1.get k, d2.get k) with
          | (some (_, v1), some (_, v2)) => eqInternal v1 v2 strict
          | _ => false)

  partial def eqInternal (first: Types) (second: Types) (strict: Bool) : Bool :=
    match first, second with
    | Types.intVal n, Types.intVal v => n == v
    | Types.intVal n, Types.floatVal v => if strict then false else (Float.ofInt n) == v
    | Types.floatVal n, Types.floatVal v => n == v
    | Types.floatVal n, Types.intVal v => if strict then false else n == (Float.ofInt v)
    | Types.strVal n, Types.strVal v => n == v
    | Types.boolVal n, Types.boolVal v => n == v
    | Types.symbolVal n, Types.symbolVal v => n == v
    | Types.keywordVal n, Types.keywordVal v => n == v
    | Types.Nil, Types.Nil => true
    | Types.listVal n, Types.listVal v =>
      if n.length != v.length then false
      else eqList n v strict
    | Types.vecVal nvec, Types.vecVal vvec =>
      let n := toList nvec
      let v := toList vvec
      if n.length != v.length then false
      else eqList n v strict
    | Types.dictVal n, Types.dictVal v => eqDict n v strict
    | Types.listVal n, Types.vecVal vvec => if strict then false else
      let v := toList vvec
      if n.length != v.length then false
      else eqList n v strict
    | Types.vecVal nvec, Types.listVal v => if strict then false else
      let n := toList nvec
      if n.length != v.length then false
      else eqList n v strict
    | _, _ => false

end

def eq (env : Env) (lst: List Types) (strict: Bool) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := eqInternal first second strict
    return (env, Types.boolVal res)

def makeAtom (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "keyword: 1 argument required")
  else
    let first := lst[0]!
    return (env, Types.atomVal (Atom.v first))

def derefAtom (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "deref: 1 argument required")
  else
    let first := lst[0]!
    match first with
    | Types.atomVal x => match x with
      | Atom.v v => return (env, v)
      | Atom.withmeta v _ => return (env, v)
    | x => throw (IO.userError s!"deref: unexpected symbol: {x.toString true}, expected: atom")

def resetAtom (env : Env) (lst: List Types) (args: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "reset!: 2 argument required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let atomSymbol := args[0]!
    match atomSymbol with
    | Types.symbolVal sym =>
      match env.get (KeyType.strKey sym) with
      | none => throw (IO.userError s!"{sym} not found")
      | some (level, _) => match first with
        | Types.atomVal x => match x with
          | Atom.v _ =>
              let newEnv := env.add (KeyType.strKey sym) level (Types.atomVal (Atom.v second))
              return (newEnv, second)
          | Atom.withmeta _ meta =>
              let newEnv := env.add (KeyType.strKey sym) level (Types.atomVal (Atom.withmeta second meta))
              return (newEnv, second)
        | x => throw (IO.userError s!"reset!: unexpected symbol: {x.toString true}, expected: atom")
    | x => throw (IO.userError s!"reset!: unexpected token: {x.toString true}, expected: symbol")

def prStrInternal (lst: List Types) (printReadably: Bool) (sep: String) : String :=
  let elems := lst.map (fun x => x.toString printReadably)
  String.intercalate sep elems

def KEY_DEBUG_EVAL := "DEBUG-EVAL"

def getDebugEval (env : Env): Bool :=
  match env.get (KeyType.strKey KEY_DEBUG_EVAL) with
    | some (_, v) => match v with
      | Types.boolVal v => v
      | Types.Nil => false
      | _ => false
    | _ => false

def prStrFunc (env : Env) (lst: List Types) : IO (Env × Types) := do
  let str := prStrInternal lst true " "
  return (env, Types.strVal str)

def prnFunc (env : Env) (lst: List Types) : IO (Env × Types) := do
  let str := prStrInternal lst true " "
  IO.println str
  return (env, Types.Nil)

def printlnFunc (env : Env) (lst: List Types) : IO (Env × Types) := do
  let str := prStrInternal lst false " "
  IO.println str
  return (env, Types.Nil)

def strFunc (env : Env) (lst: List Types) : IO (Env × Types) := do
  let str := prStrInternal lst false ""
  return (env, Types.strVal str)

def countFunc(env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "count: 1 argument required")
  else
    let x := lst[0]!
    match x with
      | Types.listVal v => return (env, Types.intVal v.length)
      | Types.vecVal v => return (env, Types.intVal (toList v).length)
      | Types.Nil => return (env, Types.intVal 0)
      | _ => throw (IO.userError "count called on non-sequence")

def readString (lst: List Types) (_: Env) : IO Types := do
  if lst.length < 1 then throw (IO.userError "read-string: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.strVal v => match read_types_with_env v with -- Dict.empty
      | Except.error e => throw (IO.userError e)
      | Except.ok res => return res
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: string")

def cons (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "cons: >= 2 arguments required")
  else
    let elem := lst[0]!
    let seq := lst[1]!
    match seq with
    | Types.listVal v => return (env, (Types.listVal (elem :: v)))
    | Types.vecVal v => return (env, (Types.listVal (elem :: (toList v))))
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")

def concat (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then return (env, Types.listVal [])
  else
    let v ← lst.foldlM (fun (acc: List Types) x =>
      match x with
        | Types.listVal v => return acc ++ v
        | Types.vecVal v => return acc ++ (toList v)
        | x => return acc ++ [x]
    ) []
    return (env, Types.listVal v)


def makeVec (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "vec: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.vecVal v => return (env, Types.vecVal v)
    | Types.listVal v => return (env, Types.vecVal (listToVec v))
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")

def nthSeq (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 2 then throw (IO.userError "nth: >= 2 arguments required")
  else
    let first := lst[0]!
    let indx := lst[1]!
    match indx with
    | Types.intVal i =>
      match first with
      | Types.vecVal v =>
        let lv := toList v
        match lv.get? i.toNat with
          | some v => return (env, v)
          | none => throw (IO.userError "nth: index out of range")
      | Types.listVal lv =>
        if lv.length <= i then throw (IO.userError s!"nth: index out of range: {i}")
        else
          match lv.get? i.toNat with
          | some v => return (env, v)
          | none => throw (IO.userError "nth: index out of range")
      | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: number")

def firstSeq (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "first: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.Nil => return (env, Types.Nil)
    | Types.vecVal v =>
      let lv := toList v
      if lv.length == 0 then return (env, Types.Nil)
      else
        let elem := lv[0]!
        return (env, elem)
    | Types.listVal lv =>
      if lv.length == 0 then return (env, Types.Nil)
      else
        let elem := lv[0]!
        return (env, elem)
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")

def restSeq (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "rest: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.Nil => return (env, Types.listVal [])
    | Types.vecVal v =>
      let lv := toList v
      if lv.length < 1 then return (env, Types.listVal [])
      else
        return (env, Types.listVal (lv.drop 1))
    | Types.listVal lv =>
      if lv.length < 1 then return (env, Types.listVal [])
      else
        return (env, Types.listVal (lv.drop 1))
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")

def makeVector (env : Env) (lst: List Types) : IO (Env × Types) := do
  return (env, Types.vecVal (listToVec lst))

def makeDictInternal (initialDict : Dict) (lst: List Types) : IO (Dict) := do
  let rec loop (lst : List Types) (acckeys: List String) (acc : Dict) : IO (Dict × List String) :=
    match lst with
    | [] => return (acc, acckeys)
    | (Types.strVal k) :: v :: rest =>
      if acckeys.contains k then return (acc, acckeys)
      else loop rest (acckeys ++ [k]) (Dict.insert (KeyType.strKey k) 0 v acc)
    | (Types.keywordVal k) :: v :: rest =>
      if acckeys.contains k then return (acc, acckeys)
      else loop rest (acckeys ++ [k]) (Dict.insert (KeyType.keywordKey k) 0 v acc)
    | _ => throw (IO.userError "Invalid list format: Expected alternating string/keyword and value")
  let (v, _) ← loop lst [] initialDict
  return v

def makeDict (env : Env) (lst: List Types) : IO (Env × Types) := do
  let newDict ← makeDictInternal Dict.empty lst
  return (env, Types.dictVal newDict)

def assocDict (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "assoc: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      let newDict ← makeDictInternal v rest
      return (env, Types.dictVal newDict)
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: hash-map")

def dissoc (dict : Dict) (keys : List Types) : IO Dict :=
  let rec loop (keys : List Types) (acc : Dict) : IO Dict :=
    match keys with
    | [] => return acc
    | key :: rest =>
      match key with
      | Types.strVal v =>
        let newDict := acc.remove (KeyType.strKey v)
        loop rest newDict
      | Types.keywordVal v =>
        let newDict := acc.remove (KeyType.strKey v)
        loop rest newDict
      | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: keyword or string")
  loop keys dict

def dissocDict (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "dissoc: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      let newDict ← dissoc v rest
      return (env, Types.dictVal newDict)
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: hash-map")

def getDict (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "get: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      match rest with
      | [] => return (env, Types.Nil)
      | _ =>
        match (rest[0]!) with
        | Types.strVal k =>
          match v.get (KeyType.strKey k) with
          | some (_, val) => return (env, val)
          | none => return (env, Types.Nil)
        | Types.keywordVal k =>
          match v.get (KeyType.keywordKey k) with
          | some (_, val) => return (env, val)
          | none => return (env, Types.Nil)
        | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: keyword or string")
    | Types.Nil => return (env, Types.Nil)
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: hash-map")

def containsDict (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "contains?: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      match rest with
        | [] => return (env, Types.boolVal false)
        | _ =>
          match (rest[0]!) with
          | Types.strVal k =>
            match v.get (KeyType.strKey k) with
            | some _ => return (env, Types.boolVal true)
            | none => return (env, Types.boolVal false)
          | Types.keywordVal k =>
            match v.get  (KeyType.strKey k) with
            | some _ => return (env, Types.boolVal true)
            | none => return (env, Types.boolVal false)
          | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: keyword or string")
    | Types.Nil => return (env, Types.boolVal false)
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: hash-map")

def getKeysDict (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "keys: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.dictVal v =>
      let keys := v.keys
      let result := keys.map (fun k =>
        match k with
        | KeyType.strKey v => (Types.strVal v)
        | KeyType.keywordKey v => (Types.keywordVal v)
      )
      return (env, (Types.listVal result))
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: hash-map")

def getValuesDict (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "get: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.dictVal v =>
      let values := v.values
      return (env, (Types.listVal values))
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: hash-map")

def makeSymbol (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "symbol: 1 argument required")
  else
    let first := lst[0]!
    match first with
    | Types.symbolVal v => return (env, Types.symbolVal v)
    | Types.strVal v => return (env, Types.symbolVal v)
    | x => throw (IO.userError s!"symbol: unexpected symbol: {x.toString true}, expected: string")

def makeKeyword (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "keyword: 1 argument required")
  else
    let first := lst[0]!
    match first with
    | Types.keywordVal v => return (env, Types.keywordVal v)
    | Types.strVal v => return (env, Types.keywordVal v)
    | x => throw (IO.userError s!"keyword: unexpected symbol: {x.toString true}, expected: string")

def conj (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "conj: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.listVal v => return (env, Types.listVal ( rest.reverse ++ v))
    | Types.vecVal v => return (env, Types.vecVal (listToVec ((toList v) ++ rest)))
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list or vector")

def seq (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then throw (IO.userError "conj: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.Nil => return (env, Types.Nil)
    | Types.listVal v => if v.length == 0 then return (env, Types.Nil) else return (env, Types.listVal v)
    | Types.vecVal vv =>
      let v := toList vv
      if v.length == 0 then return (env, Types.Nil) else return (env, Types.listVal v)
    | Types.strVal v =>
      if v.length == 0 then return (env, Types.Nil)
      else
        let lv := v.toList.map (fun x => Types.strVal (String.singleton x))
        return (env, Types.listVal lv)
    | x => throw (IO.userError s!"unexpected symbol: {x.toString true}, expected: list, vector or string")

partial def throwFn (_ : Env) (lst : List Types) : IO (Env × Types) := do
    if lst.length < 1 then throw (IO.userError "panic")
    else
      let a := lst[0]!
      match a with
      | Types.strVal v => throw (IO.userError v)
      | x => throw (IO.userError (x.toString true))

def readFileContent (filePath : String) : IO String := do
  IO.FS.readFile filePath

def slurp (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then
    throw (IO.userError "slurp: 1 argument required")
  else
    match lst[0]! with
    | Types.strVal filename => do
      let result ← try
        let content ← readFileContent filename
        return (env, Types.strVal content)
      catch e =>
        throw (IO.userError s!"slurp: failed to read file: {e.toString}")
    | _ => throw (IO.userError "slurp: filename must be a string")

def isEOF (stdin : IO.FS.Stream) : IO Bool := do
  let input ← stdin.read 1 -- Try to read one more character
  if input.isEmpty then
    pure true -- EOF detected
  else
    pure false -- Some input available

def prompt (msg: String) : IO (Option String) := do
  IO.print msg
  let stdin ← IO.getStdin
  let input ← stdin.getLine
  if input.isEmpty then
    let eof ← isEOF stdin
    if eof then
      return none -- Indicates EOF (Ctrl+D)
    else
      return some ""
  else
    let value := input.trim
    if value = "exit" then
      return some ""
    else
      return some value

def readline (env : Env) (lst: List Types) : IO (Env × Types) := do
  if lst.length < 1 then
    throw (IO.userError "readline: 1 arguments required")
  else
     match lst[0]! with
    | Types.strVal msg => do
      let ret := ← prompt msg
      match ret with
      | none => return (env, Types.Nil)
      | some v => return (env, Types.strVal v)
    | _ => throw (IO.userError "readline: argument must be a string")

def loadFnNative (env : Env) (name: String) : Env :=
  env.add (KeyType.strKey name) 0 (Types.funcVal (Fun.builtin name))

def loadFnNativeFold (env : Env) (fnNames : List String) : Env :=
  fnNames.foldl loadFnNative env

def coreFnSymbols: List String := [
  "+", "-", "*", "/",
  "<", "<=", ">", ">=", "=",
  "number?",
  "list", "list?", "empty?", "count",
  "concat", "cons",
  "vec", "nth", "first", "rest", "vector", "vector?",
  "map", "apply",
  "conj", "seq", "sequential?",
  "hash-map", "assoc", "dissoc", "get", "contains?", "keys", "vals", "map?",
  "string?",
  "throw",
  "symbol", "keyword", "symbol?", "keyword?",
  "nil?", "true?", "false?", "fn?", "macro?",
  "prn", "pr-str", "str", "println",
  "read-string", "slurp",
  "atom", "atom?", "deref", "reset!", "swap!",
  "eval",
  "readline",
  "time-ms", "meta", "with-meta"
]

def loadFnNativeAll (env : Env) : Env :=
  (
    loadFnNativeFold env coreFnSymbols
  ).add (KeyType.strKey KEY_DEBUG_EVAL) 0 (Types.boolVal false)

def setSymbol (env : Env) (name: String) (value: Types): Env :=
  env.add (KeyType.strKey name) 0 value

-- forwards mutated variables defined in outer scopes
-- outer scopes always have a lower level index
-- used to forward mutated atoms and variables defined by `eval` in the root scope
def forwardOuterScopeDefs (envSource: Env) (envOuter: Env): Env :=
  envSource.getDict.fold envOuter (fun key l v acc =>
    if l > acc.getLevel then acc
    else if l < acc.getLevel then acc.add key l v
    else
      match acc.get key with
        | none => acc.add key l v
        | some (lOuter, _) =>
          if l != lOuter then acc
          else acc.add key l v
  )
