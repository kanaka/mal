#include "yeti_regex.i"
require, "types.i"

TOKENIZER_REGEXP = regcomp("[[:space:],]*(~@|[][{}()'`~@]|\"([\\].|[^\\\"])*\"?|;[^\n]*|[^][[:space:]{}()'\"`~@,;]*)")

func tokenize(str)
{
  match0 = ""
  match1 = ""
  pos = 1
  tokens = []
  while (1) {
    m = regmatch(TOKENIZER_REGEXP, str, match0, match1, start=pos, indices=1)
    if (m == 0) break
    b = match1(1)
    e = match1(2) - 1
    if (e < b) {
      pos = match1(2) + 1
      continue
    }
    token = strpart(str, b:e)
    pos = match1(2)
    if (strpart(token, 1:1) == ";") continue
    grow, tokens, [token]
  }
  return tokens
}

struct Reader {
  pointer tokens
  int pos
}

func reader_peek(rdr)
{
  if (rdr.pos > numberof(*rdr.tokens)) return string(0)
  return (*rdr.tokens)(rdr.pos)
}

func reader_next(rdr)
{
  token = reader_peek(rdr)
  rdr.pos += 1
  return token
}

NUMBER_REGEXP = regcomp("^-?[0-9]+$")
STR_REGEXP = regcomp("^\"([\\].|[^\\\"])*\"$")
STR_BAD_REGEXP = regcomp("^\".*$")

func unescape(s)
{
  s = strpart(s, 2:-1) // remove surrounding quotes
  s = streplaceall(s, "\\\\", "\x01")
  s = streplaceall(s, "\\n", "\n")
  s = streplaceall(s, "\\\"", "\"")
  return streplaceall(s, "\x01", "\\")
}

func read_atom(rdr)
{
  token = reader_next(rdr)
  if (token == "nil") return MAL_NIL
  else if (token == "true") return MAL_TRUE
  else if (token == "false") return MAL_FALSE
  else if (regmatch(NUMBER_REGEXP, token)) return MalNumber(val=tonum(token))
  else if (regmatch(STR_REGEXP, token)) return MalString(val=unescape(token))
  else if (regmatch(STR_BAD_REGEXP, token)) return MalError(message=("expected '\"', got EOF"))
  else if (strpart(token, 1:1) == ":") return MalKeyword(val=strpart(token, 2:))
  else return MalSymbol(val=token)
}

func read_seq(rdr, start_char, end_char)
{
  token = reader_next(rdr)
  if (token != start_char) {
    return MalError(message=("expected '" + start_char + "', got EOF"))
  }

  elements = []
  token = reader_peek(rdr)
  while (token != end_char) {
    if (token == string(0)) {
      return MalError(message=("expected '" + end_char + "', got EOF"))
    }
    e = read_form(rdr)
    if (structof(e) == MalError) return e
    grow, elements, [&e]
    token = reader_peek(rdr)
  }
  token = reader_next(rdr)
  return elements
}

func read_list(rdr)
{
  seq = read_seq(rdr, "(", ")")
  if (structof(seq) == MalError) return seq
  return MalList(val=&seq)
}

func read_vector(rdr)
{
  seq = read_seq(rdr, "[", "]")
  if (structof(seq) == MalError) return seq
  return MalVector(val=&seq)
}

func read_hashmap(rdr)
{
  seq = read_seq(rdr, "{", "}")
  if (structof(seq) == MalError) return seq
  return array_to_hashmap(seq)
}

func reader_macro(rdr, symbol_name)
{
  shortcut = reader_next(rdr)
  form = read_form(rdr)
  if (structof(form) == MalError) return form
  seq = [&MalSymbol(val=symbol_name), &form]
  return MalList(val=&seq)
}

func reader_with_meta_macro(rdr)
{
  shortcut = reader_next(rdr)
  meta = read_form(rdr)
  if (structof(meta) == MalError) return meta
  form = read_form(rdr)
  if (structof(form) == MalError) return form
  seq = [&MalSymbol(val="with-meta"), &form, &meta]
  return MalList(val=&seq)
}

func read_form(rdr)
{
  token = reader_peek(rdr)
  if (token == "'") return reader_macro(rdr, "quote")
  else if (token == "`") return reader_macro(rdr, "quasiquote")
  else if (token == "~") return reader_macro(rdr, "unquote")
  else if (token == "~@") return reader_macro(rdr, "splice-unquote")
  else if (token == "@") return reader_macro(rdr, "deref")
  else if (token == "^") return reader_with_meta_macro(rdr)
  else if (token == "(") return read_list(rdr)
  else if (token == ")") return MalError(message="unexpected ')'")
  else if (token == "[") return read_vector(rdr)
  else if (token == "]") return MalError(message="unexpected ']'")
  else if (token == "{") return read_hashmap(rdr)
  else if (token == "}") return MalError(message="unexpected '}'")
  else return read_atom(rdr)
}

func read_str(str)
{
  tokens = tokenize(str)
  rdr = Reader(tokens=&tokens, pos=1)
  return read_form(rdr)
}
