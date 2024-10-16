import Lean.Data.RBMap
import Lean

set_option diagnostics true
set_option genSizeOfSpec false
set_option diagnostics.threshold 5000

universe u

inductive Vec (α : Type u) : Nat → Type u
| nil  : Vec α 0
| cons : α → Vec α n → Vec α (n + 1)
deriving Repr

-- Define the map function for Vec
def map {α : Type u} {β : Type v} {n : Nat} (f : α → β) : Vec α n → Vec β n
| Vec.nil          => Vec.nil
| Vec.cons x xs    => Vec.cons (f x) (map f xs)

-- Function to convert Vec to List
def toList {α : Type u} {n : Nat} : Vec α n → List α
| Vec.nil          => []
| Vec.cons x xs    => x :: toList xs

def listToVec  : (lst : List Types) → Vec Types lst.length
  | []      => Vec.nil
  | x :: xs => Vec.cons x (listToVec xs)

inductive KeyType
    | strKey : String → KeyType
    | keywordKey : String → KeyType
    deriving Repr

mutual

  inductive Types : Type u
    | strVal (v : String)
    | intVal (v : Int)
    | floatVal (v : Float)
    | boolVal (v : Bool)
    | symbolVal (sym: String)
    | keywordVal (key: String)
    | listVal (el : List Types)
    | funcVal (el: Fun)
    | vecVal {n : Nat} (el : Vec Types n)
    | dictVal (el : Dict)
    | atomVal (el: Atom)
    | Nil
    deriving Repr

  inductive Fun : Type u
    | builtin (name : String)
    | userDefined (env: Env) (params : Types) (body : Types)
    | macroFn (env: Env) (params : Types) (body : Types)

  inductive Dict: Type u
    | empty : Dict
    | insert: KeyType → Nat → Types → Dict → Dict
    deriving Repr

  inductive Env: Type u
  | data: Nat → Dict → Env

  inductive Atom
  | v : Types -> Atom
  | withmeta : Types → Types → Atom
  deriving Repr

end

instance : Inhabited Env where
  default := Env.data 0 Dict.empty

instance : Inhabited Dict where
  default := Dict.empty

instance : Inhabited Types where
  default := Types.Nil

instance : Inhabited (List Types) where
  default := []

instance : Inhabited (Dict × Types) where
  default := (default, default)

def Dict.get : Dict → KeyType → Option (Nat × Types)
  | Dict.empty, _ => default
  | Dict.insert k l v d, key =>
    match k, key with
    | KeyType.strKey ks, KeyType.strKey keyg => if ks = keyg then some (l, v) else d.get key
    | KeyType.keywordKey ks, KeyType.keywordKey keyg => if ks = keyg then some (l, v) else d.get key
    | KeyType.strKey ks, KeyType.keywordKey keyg => if ks = keyg then some (l, v) else d.get key
    | KeyType.keywordKey ks, KeyType.strKey keyg => if ks = keyg then some (l, v) else d.get key

def Dict.keys : Dict → List KeyType
  | Dict.empty => []
  | Dict.insert k _ _ d =>
    let restKeys := d.keys
    k :: restKeys

def Dict.values : Dict → List Types
  | Dict.empty => []
  | Dict.insert _ _ v d =>
    let restValues := d.values
    v :: restValues

def Dict.remove (d : Dict) (key : KeyType) : Dict :=
  match d with
  | Dict.empty => Dict.empty
  | Dict.insert k l v rest =>
    match k, key with
      | KeyType.strKey ks, KeyType.strKey keyg => if ks = keyg then rest.remove key else Dict.insert k l v (rest.remove key)
      | KeyType.keywordKey ks, KeyType.keywordKey keyg => if ks = keyg then rest.remove key else Dict.insert k l v (rest.remove key)
      | KeyType.strKey ks, KeyType.keywordKey keyg => if ks = keyg then rest.remove key else Dict.insert k l v (rest.remove key)
      | KeyType.keywordKey ks, KeyType.strKey keyg => if ks = keyg then rest.remove key else Dict.insert k l v (rest.remove key)

def Dict.add : Dict → KeyType → Nat → Types → Dict
  | Dict.empty, key, level, value => Dict.insert key level value Dict.empty
  | Dict.insert k _ v d, key, level, value =>
    match k, key with
      | KeyType.strKey ks, KeyType.strKey keyg => if ks = keyg then Dict.insert k level value d else Dict.insert k level v (d.add key level value)
      | KeyType.keywordKey ks, KeyType.keywordKey keyg => if ks = keyg then Dict.insert k level value d else Dict.insert k level v (d.add key level value)
      | KeyType.strKey ks, KeyType.keywordKey keyg => if ks = keyg then Dict.insert k level value d else Dict.insert k level v (d.add key level value)
      | KeyType.keywordKey ks, KeyType.strKey keyg => if ks = keyg then Dict.insert k level value d else Dict.insert k level v (d.add key level value)

-- Helper function to fold over all elements in a Dict
partial def Dict.fold (d : Dict) (init : α) (f : KeyType → Nat → Types → α → α) : α :=
  match d with
  | Dict.empty => init
  | Dict.insert k l v d' => d'.fold (f k l v init) f

-- Function to merge two Dicts.
def Dict.merge (baseDict overwriteDict : Dict) : Dict :=
  let merged := overwriteDict.fold baseDict (fun key l v acc =>
    match acc.get key with
    | some (lBase, _) =>
      if l > lBase then acc.add key l v else acc
    | none => acc.add key l v)
  merged

-- Function to extract the string from a Types.symbolVal
def getSymbol (t : Types) : Option String :=
  match t with
  | Types.symbolVal sym => some sym
  | _ => none

def getKeyword (t : Types) : Option String :=
  match t with
  | Types.keywordVal key => some key
  | _ => none

def buildDictWithSymbols (ref: Dict) (level: Nat) (keys : List String) (values : List Types) : Dict :=
  match keys, values with
  | [], _ => Dict.empty
  | _, [] => Dict.empty
  | key :: keyTail, value :: valueTail =>
    let val := match value with
    | Types.symbolVal v =>
      let entry := ref.get (KeyType.strKey v)
      match entry with
      | some (_, v) => v
      | none => Types.Nil
    | _ => value
    let restDict := buildDictWithSymbols ref level keyTail valueTail
    Dict.insert (KeyType.strKey key) level val restDict

def buildDict (level: Nat) (keys : List String) (values : List Types) : Dict :=
  match keys, values with
  | [], _ => Dict.empty
  | _, [] => Dict.empty
  | key :: keyTail, value :: valueTail =>
    let restDict := buildDict level keyTail valueTail
    Dict.insert (KeyType.strKey key) level value restDict

def Env.getLevel : Env → Nat
  | Env.data l _ => l

def Env.getDict : Env → Dict
  | Env.data _ d => d

def Env.get : Env → KeyType → Option (Nat × Types)
  | Env.data _ d, key => d.get key

def Env.keys : Env → List KeyType
  | Env.data _ d => d.keys

def Env.values : Env → List KeyType
  | Env.data _ d => d.keys

def Env.remove : Env → KeyType → Dict
  | Env.data _ d, key => d.remove key

def Env.add : Env → KeyType → Nat → Types → Env
  | Env.data l d, key, level, value => Env.data l (d.add key level value)

def Env.increment : Env → Env
  | Env.data l d => Env.data (l + 1) d

def Env.merge : Env → Env → Env
  | Env.data _ d, e2 =>  Env.data e2.getLevel (d.merge e2.getDict)

def Env.mergeDict : Env → Nat → Dict → Env
  | Env.data _ d, level2, d2 =>  Env.data level2 (d.merge d2)

def Types.toBool: Types -> Bool
  | Types.boolVal v => if v then true else false
  | Types.Nil => false
  | _ => true

def getFirst! (lst : List Types) : Types :=
  match lst with
  | []      => default
  | x :: _  => x

def escapeString (input : String) : String :=
  input.foldl (fun acc c =>
    acc ++ match c with
      | '\\' => "\\\\"
      -- | '"' => "\\\""
      | '\"' => "\\\""
      | '\n' => "\\n"
      | _ => String.singleton c
  ) ""

mutual
  partial def Types.toString (readably: Bool) (t:Types) : String :=
    match t with
    | Types.strVal v => stringToString readably v
    | Types.intVal v => s!"{v}"
    | Types.floatVal v => s!"{v}"
    | Types.boolVal v => s!"{v}"
    | Types.funcVal el => Fun.toString readably el
    -- | Types.funcVal v => "(" ++ s!"{(Types.toString v)}" ++ ")"
    | Types.listVal el => s!"({String.intercalate " " (el.map (Types.toString readably))})"
    | Types.dictVal el => "{" ++ s!"{Dict.toString readably el}" ++ "}"
    | Types.Nil => "nil"
    | Types.symbolVal sym => s!"{sym}"
    | Types.keywordVal key => s!":{key}"
    | Types.vecVal el =>
      let content := toList el
      s!"[{String.intercalate " " (content.map (Types.toString readably))}]"
    | Types.atomVal el => Atom.toString readably el

  partial def stringToString (readably: Bool) (v:String) : String :=
    if readably then s!"\"{escapeString v}\""
    else v

  partial def Atom.toString (readably: Bool) (t:Atom) : String :=
    match t with
    | Atom.v v => s!"(atom {v.toString readably})"
    | Atom.withmeta v _ => s!"(atom {v.toString readably})"

  partial def Fun.toString (readably: Bool) (t:Fun) : String :=
    match t with
    | Fun.userDefined _ params body => "(fn* " ++ s!"{(Types.toString readably params)}" ++ s!"{(Types.toString readably body)}" ++ ")"
    | Fun.macroFn _ params body => "(fn* " ++ s!"{(Types.toString readably params)}" ++ s!"{(Types.toString readably body)}" ++ ")"
    | Fun.builtin name => s!"{name}"

  partial def Dict.toString (readably: Bool) (d:Dict) : String :=
    match d with
    | Dict.empty => ""
    | Dict.insert key _ value Dict.empty =>
      match key with
      | KeyType.strKey k => s!"\"{k}\" {Types.toString readably value}"
      | KeyType.keywordKey k => s!":{k} {Types.toString readably value}"
    | Dict.insert key _ value rest =>
      let restStr := Dict.toString readably rest
      match key with
      | KeyType.strKey k => s!"{restStr} \"{k}\" {Types.toString readably value}"
      | KeyType.keywordKey k => s!"{restStr} :{k} {Types.toString readably value}"

  partial def Dict.toStringWithLevels (readably: Bool) (d:Dict) : String :=
    match d with
    | Dict.empty => ""
    | Dict.insert key l value Dict.empty =>
      match key with
      | KeyType.strKey k => s!"\"{k}\" ({l}) {Types.toString readably value}"
      | KeyType.keywordKey k => s!":{k} ({l}) {Types.toString readably value}"
    | Dict.insert key l value rest =>
      let restStr := Dict.toStringWithLevels readably rest
      match key with
      | KeyType.strKey k => s!"{restStr} \"{k}\" ({l}) {Types.toString readably value}"
      | KeyType.keywordKey k => s!"{restStr} :{k} ({l}) {Types.toString readably value}"
end

def Env.toString (readably: Bool) (e:Env) : String :=
  match e with
  | Env.data l d => s!"level: {l} dict: {d.toStringWithLevels readably}"
