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
  { SWITCHON sd%p INTO
    { CASE ' ': CASE '*t': CASE '*n': CASE ',': LOOP // Inter-token whitespace
      CASE '~': // Maybe ~@
        IF p < s!str_len & sd%(p+1) = '@' THEN
	{ tokstart := p
	  p := p + 1
	  ENDCASE
	}
      CASE '[': CASE ']': CASE '{': CASE '}': CASE '(': CASE ')': CASE '*'':
      CASE '`': CASE '^': CASE '@': // Single-character token
        tokstart := p
        ENDCASE
      CASE ';': // Comment
        p := p + 1 REPEATUNTIL p > s!str_len | sd%p = '*n'
        LOOP
      CASE '*"': // String
        tokstart := p
        p := p + 1
        UNTIL p > s!str_len | sd%p = '*"' DO
          p := p + (sd%p = '\' -> 2, 1)
        ENDCASE     
      DEFAULT: // Symbol or number
        tokstart := p
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

LET read_atom(rdr) = as_sym(reader_next(rdr))

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
