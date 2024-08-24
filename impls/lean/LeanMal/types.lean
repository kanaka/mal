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
    | userDefined (ref: Dict) (params : Types) (body : Types)
    | macroFn (ref: Dict) (params : Types) (body : Types)

  inductive Dict: Type u
    | empty : Dict
    | insert: KeyType → Types → Dict → Dict
    deriving Repr

  inductive Atom
  | v : Types -> Atom
  | withmeta : Types → Types → Atom
  deriving Repr

end

def getEntry : Dict → KeyType → Option Types
  | Dict.empty, _ => default
  | Dict.insert k v d, key =>
    match k, key with
    | KeyType.strKey ks, KeyType.strKey keyg => if ks = keyg then some v else getEntry d key
    | KeyType.keywordKey ks, KeyType.keywordKey keyg => if ks = keyg then some v else getEntry d key
    | KeyType.strKey ks, KeyType.keywordKey keyg => if ks = keyg then some v else getEntry d key
    | KeyType.keywordKey ks, KeyType.strKey keyg => if ks = keyg then some v else getEntry d key

def getKeys : Dict → List KeyType
  | Dict.empty => []
  | Dict.insert k _ d =>
    let restKeys := getKeys d
    k :: restKeys

def getValues : Dict → List Types
  | Dict.empty => []
  | Dict.insert _ v d =>
    let restValues := getValues d
    v :: restValues

def removeKey (d : Dict) (key : KeyType) : Dict :=
  match d with
  | Dict.empty => Dict.empty
  | Dict.insert k v rest =>
    match k, key with
      | KeyType.strKey ks, KeyType.strKey keyg => if ks = keyg then removeKey rest key else Dict.insert k v (removeKey rest key)
      | KeyType.keywordKey ks, KeyType.keywordKey keyg => if ks = keyg then removeKey rest key else Dict.insert k v (removeKey rest key)
      | KeyType.strKey ks, KeyType.keywordKey keyg => if ks = keyg then removeKey rest key else Dict.insert k v (removeKey rest key)
      | KeyType.keywordKey ks, KeyType.strKey keyg => if ks = keyg then removeKey rest key else Dict.insert k v (removeKey rest key)


def addEntry : Dict → KeyType → Types → Dict
  | Dict.empty, key, value => Dict.insert key value Dict.empty
  | Dict.insert k v d, key, value =>
    match k, key with
      | KeyType.strKey ks, KeyType.strKey keyg => if ks = keyg then Dict.insert k value d else Dict.insert k v (addEntry d key value)
      | KeyType.keywordKey ks, KeyType.keywordKey keyg => if ks = keyg then Dict.insert k value d else Dict.insert k v (addEntry d key value)
      | KeyType.strKey ks, KeyType.keywordKey keyg => if ks = keyg then Dict.insert k value d else Dict.insert k v (addEntry d key value)
      | KeyType.keywordKey ks, KeyType.strKey keyg => if ks = keyg then Dict.insert k value d else Dict.insert k v (addEntry d key value)

-- Function to merge two Dicts
def mergeDicts : Dict → Dict → Dict
  | d1, Dict.empty => d1  -- If the second Dict is empty, return the first Dict
  | d1, Dict.insert k v rest =>
    let d1Updated := addEntry d1 k v
    mergeDicts d1Updated rest

-- Function to extract the string from a Types.symbolVal
def getSymbol (t : Types) : Option String :=
  match t with
  | Types.symbolVal sym => some sym
  | _ => none

def getKeyword (t : Types) : Option String :=
  match t with
  | Types.keywordVal key => some key
  | _ => none

def buildDictWithSymbols (ref: Dict) (keys : List String) (values : List Types) : Dict :=
  match keys, values with
  | [], _ => Dict.empty
  | _, [] => Dict.empty
  | key :: keyTail, value :: valueTail =>
    let val := match value with
    | Types.symbolVal v =>
      let entry := getEntry ref (KeyType.strKey v)
      match entry with
      | some v => v
      | none => Types.Nil
    | _ => value
    let restDict := buildDictWithSymbols ref keyTail valueTail
    Dict.insert (KeyType.strKey key) val restDict

def buildDict (keys : List String) (values : List Types) : Dict :=
  match keys, values with
  | [], _ => Dict.empty
  | _, [] => Dict.empty
  | key :: keyTail, value :: valueTail =>
    let restDict := buildDict keyTail valueTail
    Dict.insert (KeyType.strKey key) value restDict

instance : Inhabited Dict where
  default := Dict.empty

instance : Inhabited Types where
  default := Types.Nil

instance : Inhabited (List Types) where
  default := []

instance : Inhabited (Dict × Types) where
  default := (default, default)

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
    | Dict.insert key value Dict.empty =>
      match key with
      | KeyType.strKey k => s!"\"{k}\" {Types.toString readably value}"
      | KeyType.keywordKey k => s!":{k} {Types.toString readably value}"
    | Dict.insert key value rest =>
      let restStr := Dict.toString readably rest
      match key with
      | KeyType.strKey k => s!"{restStr} \"{k}\" {Types.toString readably value}"
      | KeyType.keywordKey k => s!"{restStr} :{k} {Types.toString readably value}"



end
