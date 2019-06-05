GET "libhdr"
GET "malhdr"

// A Reader is just a pointer to a (variable) pointer to the head of
// a list of strings.

LET reader_peek(rdr) = (!rdr)!lst_first

LET reader_next(rdr) = VALOF
{ LET tok = reader_peek(rdr)
  !rdr := (!rdr)!lst_rest
  RESULTIS tok
}

LET newsubstr(s, start, end) = VALOF
{ LET len = end - start
  LET ss = alloc_str(len)
  FOR i = 1 TO len DO
    (ss + str_data) % i := (s + str_data) % (start + i - 1)
  str_setlen(ss, len)
  RESULTIS ss
}

LET tokenize(s) = VALOF
{ LET tokens, tail = empty, empty
  LET sd = s + str_data
  LET tokstart, token = ?, ?
  FOR p = 1 TO s!str_len DO
  { tokstart := p
    SWITCHON sd%p INTO
    { CASE ' ': CASE '*t': CASE '*n': CASE ',': LOOP // Inter-token whitespace
      CASE '~': // Maybe ~@
        IF p < s!str_len & sd%(p+1) = '@' THEN p := p + 1 // FALLTHROUGH
      CASE '[': CASE ']': CASE '{': CASE '}': CASE '(': CASE ')': CASE '*'':
      CASE '`': CASE '^': CASE '@': // Single-character token
        ENDCASE
      CASE ';': // Comment
        p := p + 1 REPEATUNTIL p > s!str_len | sd%p = '*n'
        LOOP
      CASE '*"': // String
        p := p + 1
        UNTIL p > s!str_len | sd%p = '*"' DO
          p := p + (sd%p = '\' -> 2, 1)
        ENDCASE     
      DEFAULT: // Symbol or number
        UNTIL p > s!str_len DO
        { SWITCHON sd%p INTO
          { CASE ' ': CASE '*t': CASE '*n': CASE ',':
            CASE '[': CASE ']': CASE '{': CASE '}': CASE '(': CASE ')':
            CASE '*'': CASE '`': CASE '~': CASE '^': CASE '@': CASE '*"':
            CASE ';':
              p := p - 1; BREAK
          }
          p := p + 1
        }
        ENDCASE
    }
    // At this point, tokstart points to the first character of the token,
    // and p points to the last character.
    token := newsubstr(s, tokstart, p + 1)
    TEST tokens = empty THEN
    { tokens := cons(token, empty)
      tail := tokens
    } ELSE
    { tail!lst_rest := cons(token, empty)
      tail := tail!lst_rest
    }
  }
  RESULTIS tokens
}

// This is for reading into a string, as opposed to read_str, which is
// for reading from a string.
LET read_string(token) = VALOF
{ LET i, o, out = 2, 0, ?
  WHILE i < token!str_len DO
  { IF (token + str_data)%i = '\' THEN i := i + 1
    i, o := i + 1, o + 1
  }
  // UNLESS i = token!str_len & (token + str_data)%i = '*"' DO
  //   throw(str_bcpl2mal("unbalanced quotes"))
  out := alloc_str(o)
  i, o := 2,  1
  WHILE i < token!str_len DO
  { LET ch = (token + str_data)%i
    IF ch = '\' THEN
    { i := i + 1
      ch := (token + str_data)%i
      IF ch = 'n' THEN ch := '*n'
    }
    (out + str_data)%o := ch
    i, o := i + 1, o + 1
  }
  str_setlen(out, o - 1)
  RESULTIS out
}

LET read_number_maybe(token) = VALOF
{ LET sd, start, negative, acc = token + str_data, 1, FALSE, 0
  IF sd%1 = '-' THEN
  { IF token!str_len = 1 THEN RESULTIS nil
    negative := TRUE
    start := 2
  }
  FOR i = start TO token!str_len DO
  { acc := acc * 10
    SWITCHON sd%i INTO
    { CASE '0': ENDCASE
      CASE '1': acc := acc + 1; ENDCASE
      CASE '2': acc := acc + 2; ENDCASE
      CASE '3': acc := acc + 3; ENDCASE
      CASE '4': acc := acc + 4; ENDCASE
      CASE '5': acc := acc + 5; ENDCASE
      CASE '6': acc := acc + 6; ENDCASE
      CASE '7': acc := acc + 7; ENDCASE
      CASE '8': acc := acc + 8; ENDCASE
      CASE '9': acc := acc + 9; ENDCASE
      DEFAULT: RESULTIS nil
    }
  }
  IF negative THEN acc := -acc
  RESULTIS alloc_int(acc)
}

LET read_atom(rdr) = VALOF
{ LET token, maybenum = reader_next(rdr), ?
  IF (token + str_data)%1 = '*"' THEN RESULTIS read_string(token)
  maybenum := read_number_maybe(token)
  UNLESS maybenum = nil RESULTIS maybenum
  RESULTIS as_sym(token)
}

LET read_list(rdr) = VALOF
{ reader_next(rdr) // Skip leading '('
  RESULTIS read_list_tail(rdr)
}

AND read_list_tail(rdr) = VALOF
  TEST (reader_peek(rdr) + str_data)%1 = ')' THEN
  { reader_next(rdr)
    RESULTIS empty
  } ELSE {
    LET first = read_form(rdr)
    LET rest = read_list_tail(rdr)
    RESULTIS cons(first, rest)
  }

AND read_form(rdr) = VALOF
  SWITCHON (reader_peek(rdr) + str_data)%1 INTO
  { CASE '(': RESULTIS read_list(rdr)
    DEFAULT: RESULTIS read_atom(rdr)
  }

LET read_str(s) = VALOF
{ LET tokens = tokenize(s)
  LET rdr = @tokens
  RESULTIS read_form(rdr)
}
