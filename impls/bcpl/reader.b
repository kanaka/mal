GET "libhdr"
GET "malhdr"

LET reader_peek(toklistp) = (!toklistp)!lst_first

LET reader_next(toklistp) = VALOF
{ LET tok = reader_peek(toklistp)
  !toklistp := (!toklistp)!lst_rest
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
        UNTIL  p > s!str_len | sd%p = '*"' DO
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

// LET read_str(s) = read_form(tokenize(s))
LET read_str(s) = tokenize(s)
