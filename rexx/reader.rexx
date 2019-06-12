#ifndef __reader__
#define __reader__

#include "types.rexx"

next_token: procedure expose pos /* next_token(str) */
  TAB = '09'x
  LF = '0A'x
  CR = '0D'x
  SEPARATOR_CHARS = TAB || LF || CR || " []{}()'`,;" || '"'
  WHITESPACE_CHARS = TAB || LF || CR || " ,"
  str = arg(1)
  token = ""
  ch = substr(str, pos, 1)
  select
    when pos(ch, WHITESPACE_CHARS) > 0 then
      pos = pos + 1
    when pos(ch, "[]{}()'`^@") > 0 then do
      pos = pos + 1
      token = ch
    end
    when ch == '~' then do
      if substr(str, pos + 1, 1) == '@' then do
        pos = pos + 2
        token = "~@"
      end
      else do
        pos = pos + 1
        token = "~"
      end
    end
    when ch == ";" then do
      do while pos <= length(str)
        ch = substr(str, pos, 1)
        if (ch == LF) | (ch == CR) then
          leave
        else
          pos = pos + 1
      end
    end
    when ch == '"' then do
      tmppos = pos + 1
      do while tmppos < length(str)
        ch = substr(str, tmppos, 1)
        select
          when ch == '"' then
            leave
          when ch == '5C'x then /* backslash */
            tmppos = tmppos + 2
          otherwise
            tmppos = tmppos + 1
        end
      end
      token = substr(str, pos, tmppos - pos + 1)
      pos = tmppos + 1
    end
    otherwise
      tmppos = pos
      do while tmppos <= length(str)
        ch = substr(str, tmppos, 1)
        if pos(ch, SEPARATOR_CHARS) > 0 then
          leave
        else
          token = token || ch
        tmppos = tmppos + 1
      end
      pos = tmppos
    end
  return token

tokenize: procedure expose tokens. /* tokenize(str) */
  str = arg(1)
  tokens. = ""
  num_of_tokens = 0
  str_to_tokenize = str
  pos = 1
  do while pos <= length(str)
    token = next_token(str_to_tokenize)
    if length(token) > 0 then do
      num_of_tokens = num_of_tokens + 1
      tokens.num_of_tokens = token
    end
  end
  tokens.0 = num_of_tokens
  return num_of_tokens

is_number: procedure /* is_number(token) */
  token = arg(1)
  ch = substr(token, 1, 1)
  DIGITS = "0123456789"
  if pos(ch, DIGITS) > 0 then return 1
  if (ch == '-') & (pos(substr(token, 2, 1), DIGITS) > 0) then return 1
  return 0

parse_string: procedure /* parse_string(token) */
  token = arg(1)
  res = substr(token, 2, length(token) - 2) /* Remove quotes */
  res = changestr("\\", res, '01'x)
  res = changestr("\n", res, '0A'x)
  res = changestr('\"', res, '"')
  res = changestr('01'x, res, '5C'x)
  return res

parse_keyword: procedure /* parse_keyword(token) */
  token = arg(1)
  return substr(token, 2)  /* Remove initial ":" */

read_atom: procedure expose values. tokens. pos err /* read_atom() */
  token = tokens.pos
  pos = pos + 1
  select
    when is_number(token) then return new_number(token)
    when token == "nil" then return new_nil()
    when token == "true" then return new_true()
    when token == "false" then return new_false()
    when substr(token, 1, 1) == ':' then return new_keyword(parse_keyword(token))
    when substr(token, 1, 1) == '"' then do
      if substr(token, length(token), 1) \== '"' then do
        end_char = '"'
        err = "expected '" || end_char || "', got EOF"
        return "ERR"
      end
      return new_string(parse_string(token))
    end
    otherwise
      return new_symbol(token)
    end

read_sequence: procedure expose values. tokens. pos err /* read_sequence(type, end_char) */
  type = arg(1)
  end_char = arg(2)
  pos = pos + 1 /* Consume the open paren */
  token = tokens.pos
  seq = ""
  do while (pos <= tokens.0) & (token \== end_char)
    element = read_form()
    if element == "ERR" then return "ERR"
    if seq == "" then
      seq = element
    else
      seq = seq || " " || element
    token = tokens.pos
    if token == "" then do
      err = "expected '" || end_char || "', got EOF"
      return "ERR"
    end
  end
  pos = pos + 1 /* Consume the close paren */
  return new_seq(type, seq)

reader_macro: procedure  expose values. tokens. pos /* reader_macro(symbol) */
  symbol = arg(1)
  pos = pos + 1 /* Consume the macro token */
  element = read_form()
  if element == "ERR" then return "ERR"
  seq = new_symbol(symbol) || " " || element
  return new_list(seq)

reader_with_meta_macro: procedure  expose values. tokens. pos /* reader_with_meta_macro() */
  pos = pos + 1 /* Consume the macro token */
  meta = read_form()
  if meta == "ERR" then return "ERR"
  element = read_form()
  if element == "ERR" then return "ERR"
  seq = new_symbol("with-meta") || " " || element || " " || meta
  return new_list(seq)

read_form: procedure expose values. tokens. pos err /* read_form() */
  token = tokens.pos
  select
    when token == "'" then return reader_macro("quote")
    when token == '`' then return reader_macro("quasiquote")
    when token == '~' then return reader_macro("unquote")
    when token == '~@' then return reader_macro("splice-unquote")
    when token == '@' then return reader_macro("deref")
    when token == '^' then return reader_with_meta_macro()
    when token == '(' then return read_sequence("list", ")")
    when token == ')' then do
      err = "unexpected ')'"
      return "ERR"
    end
    when token == '[' then return read_sequence("vect", "]")
    when token == ']' then do
      err = "unexpected ']'"
      return "ERR"
    end
    when token == '{' then return read_sequence("hash", "}")
    when token == '}' then do
      err = "unexpected '}'"
      return "ERR"
    end
    otherwise
      return read_atom()
    end

read_str: procedure expose values. err /* read_str(line) */
  line = arg(1)
  tokens. = ""
  num_of_tokens = tokenize(line)
  if num_of_tokens == 0 then
    return ""
  ast. = ""
  pos = 1
  return read_form()

#endif
