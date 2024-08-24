import Lean
import Mathlib
import LeanMal.types
import LeanMal.reader

universe u

def sum (ref : Dict := Dict.empty) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x + y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x + y))
  | _                                    => Except.error (ref, "+ operator not supported")

def sub (ref : Dict := Dict.empty) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x - y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x - y))
  | _                                    => Except.error (ref, "- operator not supported")

def mul (ref : Dict := Dict.empty) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x * y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x * y))
  | _                                    => Except.error (ref, "* operator not supported")

def div (ref : Dict := Dict.empty) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  match lst with
  | []                                   => Except.ok (ref, Types.intVal 0)
  | [Types.intVal x]                     => Except.ok (ref, Types.intVal x)
  | [Types.intVal x, Types.intVal y]     => Except.ok (ref, Types.intVal (x / y))
  | [Types.floatVal x]                   => Except.ok (ref, Types.floatVal x)
  | [Types.floatVal x, Types.floatVal y] => Except.ok (ref, Types.floatVal (x / y))
  | _                                    => Except.error (ref, "/ operator not supported")

def ltInternal (first: Types) (second: Types) (orEq: Bool) : Bool :=
  match first, second with
  | Types.intVal n, Types.intVal v => n < v || (if orEq then n == v else false)
  | Types.intVal n, Types.floatVal v => (Float.ofInt n) < v || (if orEq then (Float.ofInt n) == v else false)
  | Types.floatVal n, Types.floatVal v => n < v || (if orEq then n == v else false)
  | Types.floatVal n, Types.intVal v => n < (Float.ofInt v) || (if orEq then n == (Float.ofInt v) else false)
  | Types.strVal n, Types.strVal v => n < v || (if orEq then n == v else false)
  | _, _ => false

def lt (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := ltInternal first second false
    Except.ok (ref, Types.boolVal res)

def lte (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := ltInternal first second true
    Except.ok (ref, Types.boolVal res)

def gtInternal (first: Types) (second: Types) (orEq: Bool) : Bool :=
  match first, second with
  | Types.intVal n, Types.intVal v => n > v || (if orEq then n == v else false)
  | Types.intVal n, Types.floatVal v => (Float.ofInt n) > v || (if orEq then (Float.ofInt n) == v else false)
  | Types.floatVal n, Types.floatVal v => n > v || (if orEq then n == v else false)
  | Types.floatVal n, Types.intVal v => n > (Float.ofInt v) || (if orEq then n == (Float.ofInt v) else false)
  | Types.strVal n, Types.strVal v => n > v || (if orEq then n == v else false)
  | _, _ => false

def gt (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := gtInternal first second false
    Except.ok (ref, Types.boolVal res)

def gte (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := gtInternal first second true
    Except.ok (ref, Types.boolVal res)

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
      let keys1 := getKeys d1
      let keys2 := getKeys d2
      if keys1.length != keys2.length then false
      else
        keys1.all (fun k =>
          match (getEntry d1 k, getEntry d2 k) with
          | (some v1, some v2) => eqInternal v1 v2 strict
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
    | _, _ => false

end

def eq (ref: Dict) (lst: List Types) (strict: Bool) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "eq: 2 arguments required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let res := eqInternal first second strict
    Except.ok (ref, Types.boolVal res)

def makeAtom (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "keyword: 1 argument required")
  else
    let first := lst[0]!
    Except.ok (ref, Types.atomVal (Atom.v first))

def derefAtom (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "deref: 1 argument required")
  else
    let first := lst[0]!
    match first with
    | Types.atomVal x => match x with
      | Atom.v v => Except.ok (ref, v)
      | Atom.withmeta v _ => Except.ok (ref, v)
    | x => Except.error (ref, s!"deref: unexpected symbol: {x.toString true}, expected: atom")

def resetAtom (ref: Dict) (lst: List Types) (args: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "reset!: 2 argument required")
  else
    let first := lst[0]!
    let second := lst[1]!
    let atomSymbol := args[0]!
    match atomSymbol with
    | Types.symbolVal sym =>
      match first with
      | Types.atomVal x => match x with
        | Atom.v _ =>
          let newRef := addEntry ref (KeyType.strKey sym) (Types.atomVal (Atom.v second))
          Except.ok (newRef, second)
        | Atom.withmeta _ meta =>
          let newRef := addEntry ref (KeyType.strKey sym) (Types.atomVal (Atom.withmeta second meta))
          Except.ok (newRef, second)
      | x => Except.error (ref, s!"deref: unexpected symbol: {x.toString true}, expected: atom")
    | x => Except.error (ref, s!"deref: unexpected token: {x.toString true}, expected: symbol")

def prStrInternal (lst: List Types) (printReadably: Bool) (sep: String) : String :=
  let elems := lst.map (fun x => x.toString printReadably)
  String.intercalate sep elems

-- we avoid introducing the IO monad for logging, by just collecting the logs in the environment Dict
def KEY_LOGS_INFO := "LOGS_INFO"
def KEY_LOGS_DEBUG := "LOGS_DEBUG"
def KEY_DEBUG_EVAL := "DEBUG-EVAL"

def resetLogs (ref: Dict): Dict :=
  (
    ref.insert (KeyType.strKey KEY_LOGS_INFO) (Types.listVal [])
  ).insert (KeyType.strKey KEY_LOGS_DEBUG) (Types.listVal [])

def getLogs (ref: Dict) (type: String): List Types :=
  match getEntry ref (KeyType.strKey type) with
    | some v => match v with
      | Types.listVal loglist => loglist
      | _ => []
    | _ => []

def getDebugEval (ref: Dict): Bool :=
  match getEntry ref (KeyType.strKey KEY_DEBUG_EVAL) with
    | some v => match v with
      | Types.boolVal v => v
      | Types.Nil => false
      | _ => false
    | _ => false

def getLogsInfo (ref: Dict): List Types :=
  getLogs ref KEY_LOGS_INFO

def logInfo (ref: Dict) (msg: String): Dict :=
  let loglist := getLogs ref KEY_LOGS_INFO
  let newlogs := loglist ++ [(Types.strVal msg)]
  ref.insert (KeyType.strKey KEY_LOGS_INFO) (Types.listVal newlogs)

def prStrFunc (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  let str := prStrInternal lst true " "
  Except.ok (ref, Types.strVal str)

def prnFunc (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  let str := prStrInternal lst true " "
  let newRef := logInfo ref str
  Except.ok (newRef, Types.Nil)

def printlnFunc (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  let str := prStrInternal lst false " "
  let newRef := logInfo ref str
  Except.ok (newRef, Types.Nil)

def strFunc (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  let str := prStrInternal lst false ""
  Except.ok (ref, Types.strVal str)

def countFunc(ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "count: 1 argument required")
  else
    let x := lst[0]!
    match x with
      | Types.listVal v => Except.ok (ref, Types.intVal v.length)
      | Types.Nil => Except.ok (ref, Types.intVal 0)
      | _ => Except.error (ref, "count called on non-sequence")

def readString (lst: List Types) (envir: Dict := Dict.empty) : Except String Types :=
  if lst.length < 1 then Except.error "read-string: 1 arguments required"
  else
    let first := lst[0]!
    match first with
    | Types.strVal v => read_types_with_env v envir
    | x => Except.error s!"unexpected symbol: {x.toString true}, expected: string"

def cons (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "cons: >= 2 arguments required")
  else
    let elem := lst[0]!
    let seq := lst[1]!
    match seq with
    | Types.listVal v => Except.ok (ref, (Types.listVal (elem :: v)))
    | Types.vecVal v => Except.ok (ref, (Types.listVal (elem :: (toList v))))
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")

def concat (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.ok (ref, Types.listVal [])
  else
    match lst.foldl (fun (acc: Except (Dict × String) (List Types)) x =>
      match acc with
      | Except.error e => Except.error e
      | Except.ok newlist =>
        match x with
        | Types.listVal v => Except.ok (newlist ++ v)
        | Types.vecVal v => Except.ok (newlist ++ (toList v))
        | x => Except.ok (newlist ++ [x])
    ) (Except.ok []) with
    | Except.error e => Except.error e
    | Except.ok v => Except.ok (ref, Types.listVal v)

def makeVec (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "vec: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.vecVal v => Except.ok (ref, Types.vecVal v)
    | Types.listVal v => Except.ok (ref, Types.vecVal (listToVec v))
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")

def nthSeq (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 2 then Except.error (ref, "nth: >= 2 arguments required")
  else
    let first := lst[0]!
    let indx := lst[1]!
    match indx with
    | Types.intVal i =>
      match first with
      | Types.vecVal v =>
        let lv := toList v
        match lv.get? i.toNat with
          | some v => Except.ok (ref, v)
          | none => Except.error (ref, "nth: index out of range")
      | Types.listVal lv =>
        if lv.length <= i then Except.error (ref, s!"nth: index out of range: {i}")
        else
          match lv.get? i.toNat with
          | some v => Except.ok (ref, v)
          | none => Except.error (ref, "nth: index out of range")
      | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: number")

def firstSeq (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "first: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.Nil => Except.ok (ref, Types.Nil)
    | Types.vecVal v =>
      let lv := toList v
      if lv.length == 0 then Except.ok (ref, Types.Nil)
      else
        let elem := lv[0]!
        Except.ok (ref, elem)
    | Types.listVal lv =>
      if lv.length == 0 then Except.ok (ref, Types.Nil)
      else
        let elem := lv[0]!
        Except.ok (ref, elem)
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")

def restSeq (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "rest: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.Nil => Except.ok (ref, Types.listVal [])
    | Types.vecVal v =>
      let lv := toList v
      if lv.length < 1 then Except.ok (ref, Types.listVal [])
      else
        Except.ok (ref, Types.listVal (lv.drop 1))
    | Types.listVal lv =>
      if lv.length < 1 then Except.ok (ref, Types.listVal [])
      else
        Except.ok (ref, Types.listVal (lv.drop 1))
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")

def makeVector (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  Except.ok (ref, Types.vecVal (listToVec lst))

def makeDictInternal (initialDict : Dict) (lst: List Types) : Except String (Dict) :=
  let rec loop (lst : List Types) (acc : Dict) : Except String Dict :=
    match lst with
    | [] => Except.ok acc
    | (Types.strVal k) :: v :: rest =>
      loop rest (Dict.insert (KeyType.strKey k) v acc)
    | (Types.keywordVal k) :: v :: rest =>
      loop rest (Dict.insert (KeyType.keywordKey k) v acc)
    | _ => Except.error "Invalid list format: Expected alternating string/keyword and value"
  loop lst initialDict

def makeDict (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  match makeDictInternal Dict.empty lst with
  | Except.error e => Except.error (ref, e)
  | Except.ok (newDict) => Except.ok (ref, Types.dictVal newDict)

def assocDict (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "assoc: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      match makeDictInternal v rest with
      | Except.error e => Except.error (ref, e)
      | Except.ok (newDict) => Except.ok (ref, Types.dictVal newDict)
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: hash-map")

def dissoc (dict : Dict) (keys : List Types) : Except String Dict :=
  let rec loop (keys : List Types) (acc : Dict) : Except String Dict :=
    match keys with
    | [] => Except.ok acc
    | key :: rest =>
      match key with
      | Types.strVal v =>
        let newDict := removeKey acc (KeyType.strKey v)
        loop rest newDict
      | Types.keywordVal v =>
        let newDict := removeKey acc (KeyType.strKey v)
        loop rest newDict
      | x => Except.error s!"unexpected symbol: {x.toString true}, expected: keyword or string"
  loop keys dict

def dissocDict (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "dissoc: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      match dissoc v rest with
      | Except.error e => Except.error (ref, e)
      | Except.ok newDict => Except.ok (ref, Types.dictVal newDict)
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: hash-map")

def getDict (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "get: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      match rest with
      | [] => Except.ok (ref, Types.Nil)
      | _ =>
        match (rest[0]!) with
        | Types.strVal k =>
          match getEntry v (KeyType.strKey k) with
          | some val => Except.ok (ref, val)
          | none => Except.ok (ref, Types.Nil)
        | Types.keywordVal k =>
          match getEntry v (KeyType.keywordKey k) with
          | some val => Except.ok (ref, val)
          | none => Except.ok (ref, Types.Nil)
        | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: keyword or string")
    | Types.Nil => Except.ok (ref, Types.Nil)
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: hash-map")

def containsDict (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "contains?: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.dictVal v =>
      match rest with
        | [] => Except.ok (ref, Types.boolVal false)
        | _ =>
          match (rest[0]!) with
          | Types.strVal k =>
            match getEntry v (KeyType.strKey k) with
            | some _ => Except.ok (ref, Types.boolVal true)
            | none => Except.ok (ref, Types.boolVal false)
          | Types.keywordVal k =>
            match getEntry v  (KeyType.strKey k) with
            | some _ => Except.ok (ref, Types.boolVal true)
            | none => Except.ok (ref, Types.boolVal false)
          | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: keyword or string")
    | Types.Nil => Except.ok (ref, Types.boolVal false)
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: hash-map")

def getKeysDict (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "keys: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.dictVal v =>
      let keys := getKeys v
      let result := keys.map (fun k =>
        match k with
        | KeyType.strKey v => (Types.strVal v)
        | KeyType.keywordKey v => (Types.keywordVal v)
      )
      Except.ok (ref, (Types.listVal result))
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: hash-map")

def getValuesDict (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "get: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.dictVal v =>
      let values := getValues v
      Except.ok (ref, (Types.listVal values))
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: hash-map")

def makeSymbol (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "symbol: 1 argument required")
  else
    let first := lst[0]!
    match first with
    | Types.symbolVal v => Except.ok (ref, Types.symbolVal v)
    | Types.strVal v => Except.ok (ref, Types.symbolVal v)
    | x => Except.error (ref, s!"symbol: unexpected symbol: {x.toString true}, expected: string")

def makeKeyword (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "keyword: 1 argument required")
  else
    let first := lst[0]!
    match first with
    | Types.keywordVal v => Except.ok (ref, Types.keywordVal v)
    | Types.strVal v => Except.ok (ref, Types.keywordVal v)
    | x => Except.error (ref, s!"keyword: unexpected symbol: {x.toString true}, expected: string")

def conj (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "conj: >= 1 arguments required")
  else
    let first := lst[0]!
    let rest := lst.drop 1
    match first with
    | Types.listVal v => Except.ok (ref, Types.listVal (v ++ rest))
    | Types.vecVal v => Except.ok (ref, Types.vecVal (listToVec ((toList v) ++ rest)))
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list or vector")

def seq (ref: Dict) (lst: List Types) : Except (Dict × String) (Dict × Types) :=
  if lst.length < 1 then Except.error (ref, "conj: 1 arguments required")
  else
    let first := lst[0]!
    match first with
    | Types.Nil => Except.ok (ref, Types.Nil)
    | Types.listVal v => if v.length == 0 then Except.ok (ref, Types.Nil) else Except.ok (ref, Types.listVal v)
    | Types.vecVal vv =>
      let v := toList vv
      if v.length == 0 then Except.ok (ref, Types.Nil) else Except.ok (ref, Types.listVal v)
    | Types.strVal v =>
      if v.length == 0 then Except.ok (ref, Types.Nil)
      else
        let lv := v.toList.map (fun x => Types.strVal (String.singleton x))
        Except.ok (ref, Types.listVal lv)
    | x => Except.error (ref, s!"unexpected symbol: {x.toString true}, expected: list, vector or string")

partial def throwFn (ref: Dict) (lst : List Types) : Except (Dict × String) (Dict × Types) :=
    if lst.length < 1 then Except.error (ref, "panic")
    else
      let a := lst[0]!
      match a with
      | Types.strVal v => Except.error (ref, v)
      | x => Except.error (ref, x.toString true)

def readFileContent (filePath : String) : IO String := do
  IO.FS.readFile filePath

def slurp (ref: Dict) (lst: List Types) : IO (Except (Dict × String) (Dict × Types)) := do
  if lst.length < 1 then
    return Except.error (ref, "slurp: 2 arguments required")
  else
    match lst[0]! with
    | Types.strVal filename => do
      let result ← try
        let content ← readFileContent filename
        return Except.ok (ref, Types.strVal content)
      catch e =>
        return Except.error (ref, s!"slurp: failed to read file: {e.toString}")

      -- return result
    | _ =>
      return Except.error (ref, "slurp: filename must be a string")

def slurp2 (ref: Dict) (lst: List Types) : IO (Dict × Types) := do
  if lst.length < 1 then
    throw (IO.userError "slurp: 2 arguments required")
  else
    match lst[0]! with
    | Types.strVal filename => do
      let content ← readFileContent filename
      return (ref, Types.strVal content)
    | _ =>
      throw (IO.userError "slurp: filename must be a string")

-- IO monad limits some of the formal proving capabilities that Lean offers because IO introduces side effects that are inherently non-deterministic and impure, such as reading from files
def evalFnNativeWithIO (ref : Dict := Dict.empty) (name: String) (results: List Types): IO (Except (Dict × String) (Dict × Types)) :=
  match name with
  | "slurp" => slurp ref results
  | _   => return Except.error (ref, s!"'{name}' not found")

def loadFnNative (ref: Dict) (name: String) : Dict :=
  ref.insert (KeyType.strKey name) (Types.funcVal (Fun.builtin name))

def loadFnNativeFold (ref: Dict) (fnNames : List String) : Dict :=
  fnNames.foldl loadFnNative ref

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
]

def loadFnNativeAll (ref: Dict) : Dict :=
  let newRef := loadFnNativeFold ref coreFnSymbols
  ((
    newRef.insert (KeyType.strKey KEY_LOGS_INFO) (Types.listVal [])
  ).insert (KeyType.strKey KEY_LOGS_DEBUG) (Types.listVal [])
  ).insert (KeyType.strKey KEY_DEBUG_EVAL) (Types.boolVal false)

def setSymbol (ref: Dict) (name: String) (value: Types): Dict :=
  let newRef := loadFnNative ref name
  newRef.insert (KeyType.strKey name) value
