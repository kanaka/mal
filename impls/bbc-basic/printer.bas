REM > printer library for mal in BBC BASIC

DEF FNpr_str(val%, print_readably%)
  LOCAL ret%, term$, val$, keys%, vals%
  IF FNis_nil(val%) THEN =FNalloc_string("nil")
  IF FNis_boolean(val%) THEN
    IF FNunbox_boolean(val%) THEN =FNalloc_string("true")
    =FNalloc_string("false")
  ENDIF
  IF FNis_int(val%) THEN =FNalloc_string(STR$(FNunbox_int(val%)))
  IF FNis_string(val%) THEN
    IF FNstring_chr(val%, 1) = CHR$(127) THEN =FNalloc_string(":" + MID$(FNunbox_string(val%), 2))
    IF print_readably% THEN =FNalloc_string(FNformat_string(FNunbox_string(val%))) ELSE =val%
  ENDIF
  IF FNis_symbol(val%) THEN =FNalloc_string(FNunbox_symbol(val%))
  IF FNis_corefn(val%) OR FNis_fn(val%) THEN =FNalloc_string("#<function>")
  IF FNis_seq(val%) THEN
    IF FNis_vector(val%) THEN
      ret% = FNalloc_string("[") : term$ = "]"
    ELSE
      ret% = FNalloc_string("(") : term$ = ")"
    ENDIF
    WHILE NOT FNis_empty(val%)
      IF FNstring_len(ret%) > 1 THEN ret% = FNstring_append(ret%, " ")
      ret% = FNstring_concat(ret%, FNpr_str(FNfirst(val%), print_readably%))
      val% = FNrest(val%)
    ENDWHILE
    =FNstring_append(ret%, term$)
  ENDIF
  IF FNis_hashmap(val%) THEN
    ret% = FNalloc_string("{")
    keys% = FNhashmap_keys(val%)
    vals% = FNhashmap_vals(val%)
    WHILE NOT FNis_empty(keys%)
      IF FNstring_len(ret%) > 1 THEN ret% = FNstring_append(ret%, " ")
      ret% = FNstring_concat(ret%, FNpr_str(FNfirst(keys%), print_readably%))
      ret% = FNstring_append(ret%, " ")
      ret% = FNstring_concat(ret%, FNpr_str(FNfirst(vals%), print_readably%))
      keys% = FNrest(keys%)
      vals% = FNrest(vals%)
    ENDWHILE
    =FNstring_append(ret%, "}")
  ENDIF
  IF FNis_atom(val%) THEN
    ret% = FNalloc_string("(atom ")
    ret% = FNstring_concat(ret%, FNpr_str(FNatom_deref(val%), print_readably%))
    =FNstring_append(ret%, ")")
  ENDIF
  ERROR &40E809F0, "Unprintable value"

DEF FNformat_string(strval$)
  LOCAL ptr%, c$, out$
  IF strval$ = "" THEN =""""""
  FOR ptr% = 1 TO LEN(strval$)
    c$ = MID$(strval$, ptr%, 1)
    CASE c$ OF
      WHEN "\", """": out$ += "\" + c$
      WHEN CHR$(10): out$ += "\n"
      OTHERWISE: out$ += c$
    ENDCASE
  NEXT ptr%
="""" + out$ + """"

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
