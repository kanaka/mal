REM > reader library for mal in BBC BASIC

REM  ** Reader **

REM  The Reader object is implemented as an array and a mutable pointer.

DEF FNreader_peek(tokens$(), RETURN tokptr%)
=tokens$(tokptr%)

DEF FNreader_next(token$(), RETURN tokptr%)
  tokptr% += 1
=tokens$(tokptr% - 1)

DEF FNread_str(src%)
  LOCAL ntokens%, tokptr%, tokens$()
  DIM tokens$(2048)
  ntokens% = FNtokenize(src%, tokens$())
  tokptr% = 0
=FNread_form(tokens$(), tokptr%)

REM  ** Tokenizer **

DEF FNtokenize(src%, tokens$())
  REM  The tokenizer is implemented explicitly as a deterministic
  REM  finite automaton.
  LOCAL p%, state%, tok$, tokptr%, c$, rc$, match$, action%
  LOCAL DATA

  state% = 1
  tokptr% = 0
  tok$ = ""
  FOR p% = 1 TO FNstring_len(src%)
    c$ = FNstring_chr(src%, p%)
    rc$ = c$
    REM  Convert some characters to ones that are easier to put into
    REM  DATA statements.  These substitutions are only used for
    REM  matching: the token still contains the original character.
    CASE ASC(c$) OF
      REM  Fold some upper-case letters together so that we can re-use
      REM  them to represent more awkward characters.
      WHEN 78, 81: c$ = "A"
      REM  Now convert newlines into "N"
      WHEN 10: c$ = "N"
      REM  These are the other characters that Perl's "\s" escape matches.
      WHEN 9, 11, 12, 13: c$ = " "
      REM  Brandy has a bug whereby it doesn't correctly parse strings
      REM  in DATA statements that begin with quotation marks, so convert
      REM  quotation marks to "Q".
      WHEN 34: c$ = "Q"
    ENDCASE
    REM  The state table consists of a DATA statement for each current
    REM  state, which triples representing transitions.  Each triple
    REM  consists of a string of characters to match, an action, and a
    REM  next state.  A matching string of "" matches any character,
    REM  and hence marks the end of a state.

    REM  Actions are:
    REM   0: Add this character to the current token
    REM   1: Emit token; start a new token with this character
    REM   5: Emit token; skip this character

    RESTORE +state%
    REM  state 1: Initial state, or inside a bare word
    DATA " N,",5,1,          "~",1,5, "[]{}()'`^@",1,3, Q,1,7, ";",5,11, "",0,1
    REM  state 3: Just seen the end of a token
    DATA " N,",5,1,          "~",1,5, "[]{}()'`^@",1,3, Q,1,7, ";",5,11, "",1,1
    REM  state 5: Just seen a "~"
    DATA " N,",5,1, "@",0,3, "~",1,5, "[]{}()'`^@",1,3, Q,1,7, ";",5,11, "",1,1
    REM  state 7: Inside a quoted string
    DATA "\",0,9, Q,0,3, "",0,7
    REM  state 9: After a backslash in a string
    DATA "",0,7
    REM  state 11: Inside a comment
    DATA N,5,3, "",5,11

    REM  Find a matching transition from the current state.
    REM PRINT ;state%;"-->";
    REPEAT
      READ match$, action%, state%
      REM PRINT "[";match$;"](";action%;",";state%;")";
    UNTIL match$ = "" OR INSTR(match$, c$) > 0
    REM PRINT ;"-->";state%

    REM  Execute any actions.
    IF action% AND 1 AND tokens$(tokptr%) <> "" THEN tokptr% += 1
    IF (action% AND 4) = 0 THEN tokens$(tokptr%) += rc$
  NEXT p%
  IF tokens$(tokptr%) <> "" THEN tokptr% += 1
=tokptr%        

REM  ** More Reader **

DEF FNread_form(tokens$(), RETURN tokptr%)
  LOCAL tok$, x%
  tok$ = FNreader_peek(tokens$(), tokptr%)
  CASE tok$ OF
    WHEN "" : ERROR &40E80930, "Unexpected end of input"
    WHEN "(": =FNread_list(tokens$(), tokptr%)
    WHEN "[": =FNread_vector(tokens$(), tokptr%)
    WHEN "{": =FNread_hashmap(tokens$(), tokptr%)
    WHEN ")", "]", "}": ERROR &40E80931, "Unexpected '"+tok$ +"'"
    WHEN "'": =FNreader_macro("quote", tokens$(), tokptr%)
    WHEN "`": =FNreader_macro("quasiquote", tokens$(), tokptr%)
    WHEN "~": =FNreader_macro("unquote", tokens$(), tokptr%)
    WHEN "~@":=FNreader_macro("splice-unquote", tokens$(), tokptr%)
    WHEN "@": =FNreader_macro("deref", tokens$(), tokptr%)
    WHEN "^": =FNread_with_meta(tokens$(), tokptr%)
  ENDCASE
=FNread_atom(tokens$(), tokptr%)

DEF FNread_list(tokens$(), RETURN tokptr%)
  LOCAL tok$
  tok$ = FNreader_next(tokens$(), tokptr%) : REM skip over "("
=FNread_list_tail(tokens$(), tokptr%, ")")

DEF FNread_vector(tokens$(), RETURN tokptr%)
  LOCAL tok$
  tok$ = FNreader_next(tokens$(), tokptr%) : REM skip over "["
=FNas_vector(FNread_list_tail(tokens$(), tokptr%, "]"))

DEF FNread_list_tail(tokens$(), RETURN tokptr%, term$)
  LOCAL tok$, car%, cdr%
  IF FNreader_peek(tokens$(), tokptr%) = term$ THEN
     tok$ = FNreader_next(tokens$(), tokptr%)
    =FNempty
  ENDIF
  car% = FNread_form(tokens$(), tokptr%)
  cdr% = FNread_list_tail(tokens$(), tokptr%, term$)
=FNalloc_pair(car%, cdr%)

DEF FNread_hashmap(tokens$(), RETURN tokptr%)
  LOCAL tok$, map%, key%, val%
  tok$ = FNreader_next(tokens$(), tokptr%) : REM skip over "{"
  map% = FNempty_hashmap
  WHILE FNreader_peek(tokens$(), tokptr%) <> "}"
    key% = FNread_form(tokens$(), tokptr%)
    IF NOT FNis_string(key%) ERROR &40E80932, "Hash-map key must be a string"
    val% = FNread_form(tokens$(), tokptr%)
    map% = FNhashmap_set(map%, FNunbox_string(key%), val%)
  ENDWHILE
  tok$ = FNreader_next(tokens$(), tokptr%) : REM skip over "}"
=map%
  
DEF FNreader_macro(quote$, token$(), RETURN tokptr%)
  LOCAL tok$
  tok$ = FNreader_next(tokens$(), tokptr%) : REM skip quoting token
=FNalloc_list2(FNalloc_symbol(quote$), FNread_form(tokens$(), tokptr%))

DEF FNread_with_meta(token$(), RETURN tokptr%)
  LOCAL tok$, wm%, base%, meta%
  tok$ = FNreader_next(tokens$(), tokptr%) : REM skip '^' token
  wm% = FNalloc_symbol("with-meta")
  meta% = FNread_form(tokens$(), tokptr%)
  base% = FNread_form(tokens$(), tokptr%)
=FNalloc_list3(wm%, base%, meta%)

DEF FNis_token_numeric(tok$)
  LOCAL i%, c%
  IF LEFT$(tok$, 1) = "-" THEN tok$ = MID$(tok$, 2)
  IF LEN(tok$) = 0 THEN =FALSE
  FOR i% = 1 TO LEN(tok$)
    c% = ASC(MID$(tok$, i%, 1))
    IF c% < &30 OR c% > &39 THEN =FALSE
  NEXT i%
=TRUE

DEF FNread_atom(tokens$(), RETURN tokptr%)
  LOCAL strval$
  strval$ = FNreader_next(tokens$(), tokptr%)
  IF strval$ = "nil" THEN =FNnil
  IF strval$ = "true" THEN =FNalloc_boolean(TRUE)
  IF strval$ = "false" THEN =FNalloc_boolean(FALSE)
  IF LEFT$(strval$, 1) = """" THEN =FNalloc_string(FNunquote_string(strval$))
  IF LEFT$(strval$, 1) = ":" THEN =FNalloc_string(CHR$(127) + MID$(strval$, 2))
  IF FNis_token_numeric(strval$) THEN =FNalloc_int(VAL(strval$))
=FNalloc_symbol(strval$)

DEF FNunquote_string(strval$)
  LOCAL inptr%, bs%, out$, c$
  IF RIGHT$(strval$, 1) <> """" THEN ERROR &40E80930, "Unexpected end of input"
  inptr% = 2
  REPEAT
    bs% = INSTR(strval$, "\", inptr%)
    IF bs% > 0 THEN
      out$ += MID$(strval$, inptr%, bs% - inptr%)
      c$ = MID$(strval$, bs% + 1, 1)
      IF c$ = "n" THEN c$ = CHR$(10)
      out$ += c$
      inptr% = bs% + 2
    ENDIF
  UNTIL bs% = 0
  IF inptr% = LEN(strval$) + 1 THEN ERROR &40E80930, "Unexpected end of input"
  out$ += MID$(strval$, inptr%, LEN(strval$) - inptr%)
=out$

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
