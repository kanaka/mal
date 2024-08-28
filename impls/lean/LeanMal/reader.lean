import Lean
import LeanMal.types

open Lean Lean.Parsec

universe u

-- parser for optional whitespace
def wspace : Parsec Unit :=
  many (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r') *> pure ()

def wspace_or_comma_strict : Parsec Unit :=
  many1 (pchar ' ' <|> pchar ',' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r') *> pure ()

-- custom `sep_by` combinator
partial def sep_by {α β : Type} (p : Parsec α) (sep : Parsec β) : Parsec (Array α) := do
  let mut res := #[]
  let first ← optional p
  match first with
  | none => pure res
  | some x =>
    res := res.push x
    while true do
      let next ← optional (sep *> p)
      match next with
      | none => break
      | some y => res := res.push y
    pure res


def int_to_float (n : Int) : Float :=
  if n >= 0 then
    Float.ofNat n.toNat
  else
    -Float.ofNat (-n).toNat

def read_float (intPart fracPart expPart : String) (sign : Option Char) : Option Float :=
  let fullnum := (intPart ++ fracPart).toNat!
  let plc := fracPart.length
  let exponent := expPart.toInt!
  let floatVal := Float.ofScientific fullnum true plc
  let adjustedFloat := floatVal * Float.pow 10.0 (int_to_float exponent)
  match sign with
  | some '-' => some (-adjustedFloat)
  | _        => some adjustedFloat

def read_str_val : Parsec Types := do
  let _ ← pchar '"'
  let str ← manyChars (do
    let c ← satisfy (λ c => c ≠ '"')
    if c = '\\' then
      let nextChar ← anyChar
      match nextChar with
      | '"' => pure '"'
      | '\\' => pure '\\'
      | 'n' => pure '\n'
      | 't' => pure '\t'
      | _ => fail s!"Invalid escape sequence: \\{nextChar}"
    else
      pure c
  )
  let _ ← pchar '"'
  return Types.strVal str

def is_symbol_char (c: Char): Bool :=
  c.isAlphanum || c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>' || c == ':' || c == '_' || c == '!' || c == '?' || c == '&'

def read_symbol_val : Parsec Types := do
  ws
    let sym ← many1Chars (satisfy (λ c => is_symbol_char c))
  ws
  return Types.symbolVal sym

def read_bool_val : Parsec Types := do
  ws
  let b ← (pstring "true" <|> pstring "false")
  let boolVal := Types.boolVal (b == "true")
  let nextChar ← peek?
  match nextChar with
  | none => return boolVal
  | some v =>
    if is_symbol_char v then
      let rest ← read_symbol_val
      match rest with
      | Types.symbolVal x => return Types.symbolVal (b ++ x)
      | _ => return boolVal
    else
      return boolVal

def read_nil_val : Parsec Types := do
  ws
  let _ ← pstring "nil"
  let nextChar ← peek?
  match nextChar with
  | none => return Types.Nil
  | some v =>
    if is_symbol_char v then
      let rest ← read_symbol_val
      match rest with
      | Types.symbolVal x => return Types.symbolVal ("nil" ++ x)
      | _ => return Types.Nil
    else
      return Types.Nil

def read_keyword : Parsec Types := do
  let _ ← pstring ":"
  let rest ← read_symbol_val
  match rest with
  | Types.symbolVal x => return Types.keywordVal x
  | _ => fail "not keyword"

def read_float_or_int_internal (sign: Option Char) : Parsec Types := do
  let intPart ← many1Chars digit
  optional (pchar '.') >>= fun
  | some _ => do
    let fracPart ← many1Chars digit
    optional (pchar 'e' <|> pchar 'E') >>= fun
    | some _ => do
      let expPart ← manyChars (pchar '+' <|> pchar '-' <|> digit)
      let floatStr := intPart ++ "." ++ fracPart ++ "e" ++ expPart
      match read_float intPart fracPart expPart sign with
      | some f => return Types.floatVal f
      | none   => fail s!"Invalid float: {floatStr}"
    | none => do
      let floatStr := intPart ++ "." ++ fracPart
      match read_float intPart fracPart "0" sign with
      | some f => return Types.floatVal f
      | none   => fail s!"Invalid float: {floatStr}"
  | none => do
    let intVal := intPart.toInt!
    return Types.intVal (match sign with | some '-' => -intVal | _ => intVal)

def read_float_or_int : Parsec Types := do
  let sign ← optional (pchar '+' <|> pchar '-')
  read_float_or_int_internal sign

def read_operator_or_number : Parsec Types := do
  let sign ← (pchar '+' <|> pchar '-')
  let nextChar ← peek?
  match nextChar with
  | some c =>
    if c.isWhitespace then return Types.symbolVal (String.singleton sign)
    else if c.isDigit then read_float_or_int_internal sign
    else if is_symbol_char c then
      let rest ← read_symbol_val
      match rest with
      | Types.symbolVal x => return Types.symbolVal (String.singleton sign ++ x)
      | _ => return Types.symbolVal (String.singleton sign)
    else return Types.symbolVal (String.singleton sign)
  | none => return Types.symbolVal (String.singleton sign)

mutual
  partial def read_list (envir: Dict := Dict.empty) : Parsec Types := do
    -- ws
    let _ ← optional  wspace_or_comma_strict
    let _ ← pstring "("
    let _ ← optional  wspace_or_comma_strict
    let els ← many (do
      let e ← read_types envir
      let _ ← optional  wspace_or_comma_strict
      -- let _ ← optional (pchar ',')
      return e)
    -- ws
    let _ ← optional  wspace_or_comma_strict
    let _ ← pchar ')'
    let _ ← optional  wspace_or_comma_strict
    return Types.listVal (els.toList)

    partial def read_vector (envir: Dict := Dict.empty) : Parsec Types := do
    let _ ← optional  wspace_or_comma_strict
    let _ ← pchar '['
    let _ ← optional  wspace_or_comma_strict
    let els ← many (do
      let e ← read_types envir
      let _ ← optional  wspace_or_comma_strict
      -- let _ ← optional (pchar ',')
      return e)
    let _ ← optional  wspace_or_comma_strict
    let _ ← pchar ']'
    let _ ← optional  wspace_or_comma_strict
    let vecLst := els.toList
    let vec  := listToVec vecLst
    return Types.vecVal vec

  partial def read_hash_map (_: Dict := Dict.empty) : Parsec Types := do
    let _ ← optional  wspace_or_comma_strict
    let _ ← pchar '{'
    let _ ← optional  wspace_or_comma_strict
    let els ← sep_by read_hash_map_pair (wspace *> optional (pchar ',') *> wspace)
    let _ ← optional  wspace_or_comma_strict
    let _ ← pchar '}'
    let _ ← optional  wspace_or_comma_strict
    let dict := Array.foldl (fun m (k, v) =>

      m.insert k 0 v
    ) (Dict.empty) els
    return Types.dictVal dict

  -- A parser for key-value pairs (String, Int in this case)
  partial def read_hash_map_pair : Parsec (KeyType × Types) := do
    let _ ← optional  wspace_or_comma_strict
    let key ← read_keyword <|> read_str_val
    let _ ← optional  wspace_or_comma_strict
    let value ← read_types
    let _ ← optional  wspace_or_comma_strict

    match key with
      | Types.keywordVal v => return (KeyType.keywordKey v, value)
      | Types.strVal v => return (KeyType.strKey v, value)
      | _ => default

  partial def read_symbol (chars: String) (name: String) (envir: Dict := Dict.empty) : Parsec Types := do
    let _ ← optional  wspace_or_comma_strict
    let _ ← pstring chars
    let elem ← read_types envir
    let _ ← optional  wspace_or_comma_strict

    let vecLst := [(Types.symbolVal name), elem]
    return Types.listVal vecLst

  partial def read_with_meta (envir: Dict := Dict.empty) : Parsec Types := do
    ws
    let _ ← pstring "^"

    let els ← many (do
      let e ← read_types envir
      ws
      let _ ← optional (pchar ',')
      return e)

    let elsVec := els.toList
    let vecLst := (Types.symbolVal "with-meta") :: elsVec
    return Types.listVal (List.append vecLst elsVec)

  partial def read_atom (_: Dict := Dict.empty) : Parsec Types :=
    read_operator_or_number <|> read_float_or_int <|> read_str_val <|> read_keyword <|> read_nil_val <|> read_bool_val <|> read_symbol_val

  partial def read_types (envir: Dict := Dict.empty) : Parsec Types :=
      read_list envir <|> read_vector envir <|> read_hash_map envir  <|> read_symbol "'" "quote" envir <|> read_symbol "`" "quasiquote" envir <|> read_symbol "~@" "splice-unquote" envir <|> read_symbol "~" "unquote" envir <|> read_symbol "@" "deref" envir <|> read_with_meta envir <|> read_atom envir
end

def read_types_with_env (input : String) (envir: Dict := Dict.empty)  : Except String Types :=
  match read_types envir input.trim.iter with
  | Lean.Parsec.ParseResult.success _ res => Except.ok res
  | Lean.Parsec.ParseResult.error _ err => Except.error err

def read_str (input : String)  : Except String Types :=
  read_types_with_env input (Dict.empty : Dict.{u})
