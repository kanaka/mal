import re, strutils, sequtils, types

let
  tokenRE = re"""[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)"""
  intRE   = re"-?[0-9]+$"

type
  Blank* = object of Exception

  Reader = object
    tokens: seq[string]
    position: int

proc next(r: var Reader): string =
  if r.position >= r.tokens.len:
    result = nil
  else:
    result = r.tokens[r.position]
    inc r.position

proc peek(r: Reader): string =
  if r.position >= r.tokens.len: nil
  else: r.tokens[r.position]

proc tokenize(str: string): seq[string] =
  result = @[]
  var pos = 0
  while pos < str.len:
    var matches: array[2, string]
    var len = str.findBounds(tokenRE, matches, pos)
    if len.first != -1 and len.last != -1:
      pos = len.last + 1
      if matches[0][0] != ';':
        result.add matches[0]
    else:
      inc pos

proc read_form(r: var Reader): MalType

proc read_seq(r: var Reader, fr, to: string): seq[MalType] =
  result = @[]
  var t = r.next
  if t != fr: raise newException(ValueError, "expected '" & fr & "'")

  t = r.peek
  while t != to:
    if t == nil: raise newException(ValueError, "expected '" & to & "', got EOF")
    result.add r.read_form
    t = r.peek
  discard r.next

proc read_list(r: var Reader): MalType =
  result = list r.read_seq("(", ")")

proc read_vector(r: var Reader): MalType =
  result = vector r.read_seq("[", "]")

proc read_hash_map(r: var Reader): MalType =
  result = hash_map r.read_seq("{", "}")

proc read_atom(r: var Reader): MalType =
  let t = r.next
  if t.match(intRE): number t.parseInt
  elif t[0] == '"':  str t[1 .. <t.high].replace("\\\"", "\"")
  elif t[0] == ':':  keyword t[1 .. t.high]
  elif t == "nil":   nilObj
  elif t == "true":  trueObj
  elif t == "false": falseObj
  else:              symbol t

proc read_form(r: var Reader): MalType =
  if r.peek[0] == ';':
    discard r.next
    return nilObj
  case r.peek
  of "'":
    discard r.next
    result = list(symbol "quote", r.read_form)
  of "`":
    discard r.next
    result = list(symbol "quasiquote", r.read_form)
  of "~":
    discard r.next
    result = list(symbol "unquote", r.read_form)
  of "~@":
    discard r.next
    result = list(symbol "splice-unquote", r.read_form)
  of "^":
    discard r.next
    let meta = r.read_form
    result = list(symbol "with-meta", r.read_form, meta)
  of "@":
    discard r.next
    result = list(symbol "deref", r.read_form)

  # list
  of "(": result = r.read_list
  of ")": raise newException(ValueError, "unexpected ')'")

  # vector
  of "[": result = r.read_vector
  of "]": raise newException(ValueError, "unexpected ']'")

  # hash-map
  of "{": result = r.read_hash_map
  of "}": raise newException(ValueError, "unexpected '}'")

  # atom
  else:   result = r.read_atom

proc read_str*(str: string): MalType =
  var r = Reader(tokens: str.tokenize)
  if r.tokens.len == 0:
    raise newException(Blank, "Blank line")
  r.read_form
