REM Step 2 of mal in BBC BASIC

LIBRARY "types"
LIBRARY "reader"
LIBRARY "printer"

PROCtypes_init

REM  These correspond with the CASE statement in FNcore_call
repl_env% = FNempty_hashmap
repl_env% = FNhashmap_set(repl_env%, "+", FNalloc_corefn(0))
repl_env% = FNhashmap_set(repl_env%, "-", FNalloc_corefn(1))
repl_env% = FNhashmap_set(repl_env%, "*", FNalloc_corefn(2))
repl_env% = FNhashmap_set(repl_env%, "/", FNalloc_corefn(3))

sav% = FNgc_save
REPEAT
  REM  Catch all errors apart from "Escape".
  ON ERROR LOCAL IF ERR = 17 ON ERROR OFF: ERROR ERR, REPORT$ ELSE PRINT REPORT$
  PROCgc_restore(sav%)
  sav% = FNgc_save
  PRINT "user> ";
  LINE INPUT "" line$
  PRINT FNrep(line$)
UNTIL FALSE

END

DEF FNREAD(a$)
=FNread_str(FNalloc_string(a$))

DEF FNEVAL(ast%, env%)
  LOCAL car%, val%, key$
  REM PRINT "EVAL: " + FNunbox_string(FNpr_str(ast%, TRUE))
  IF FNis_symbol(ast%) THEN
    val% = FNhashmap_get(env%, FNunbox_symbol(ast%))
    IF val% = FNnil THEN ERROR &40E80922, "Symbol not in environment"
    =val%
  ENDIF
  IF FNis_hashmap(ast%) THEN
    val% = FNempty_hashmap
    bindings% = FNhashmap_keys(ast%)
    WHILE NOT FNis_empty(bindings%)
      key$ = FNunbox_string(FNfirst(bindings%))
      val% = FNhashmap_set(val%, key$, FNEVAL(FNhashmap_get(ast%, key$), env%))
      bindings% = FNrest(bindings%)
    ENDWHILE
    =val%
  ENDIF
  IF NOT FNis_seq(ast%) THEN =ast%
  IF FNis_empty(ast%) THEN =ast%
  car% = FNEVAL(FNfirst(ast%), env%)
  IF FNis_vector(ast%) THEN =FNalloc_vector_pair(car%, FNeval_ast(FNrest(ast%), env%))
  =FNcore_call(FNunbox_corefn(car%), FNeval_ast(FNrest(ast%), env%))

DEF FNPRINT(a%)
=FNunbox_string(FNpr_str(a%, TRUE))

DEF FNrep(a$)
=FNPRINT(FNEVAL(FNREAD(a$), repl_env%))

DEF FNeval_ast(ast%, env%)
    IF FNis_empty(ast%) THEN =ast%
    =FNalloc_pair(FNEVAL(FNfirst(ast%), env%), FNeval_ast(FNrest(ast%), env%))

REM  Call a core function, taking the function number and a mal list of
REM  objects to pass as arguments.
DEF FNcore_call(fn%, args%)
  LOCAL x%, y%, z%
  x% = FNunbox_int(FNfirst(args%))
  y% = FNunbox_int(FNfirst(FNrest(args%)))
  CASE fn% OF
    WHEN 0 : z% = x%  +  y%
    WHEN 1 : z% = x%  -  y%
    WHEN 2 : z% = x%  *  y%
    WHEN 3 : z% = x% DIV y%
    OTHERWISE : ERROR &40E809F1, "Call to non-existent core function"
  ENDCASE
=FNalloc_int(z%)

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
