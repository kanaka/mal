REM Step 4 of mal in BBC BASIC

LIBRARY "types"
LIBRARY "reader"
LIBRARY "printer"
LIBRARY "env"
LIBRARY "core"

PROCtypes_init

repl_env% = FNalloc_environment(FNnil)
PROCcore_ns : REM This sets the data pointer
REPEAT
  READ sym$, i%
  IF sym$ <> "" THEN
    PROCenv_set(repl_env%, FNalloc_symbol(sym$), FNalloc_corefn(i%))
  ENDIF
UNTIL sym$ = ""

val$ = FNrep("(def! not (fn* (a) (if a false true)))")

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
  PROCgc
  PROCgc_enter
=FNgc_exit(FNEVAL_(ast%, env%))

DEF FNEVAL_(ast%, env%)
  LOCAL car%
  IF NOT FNis_list(ast%) THEN =FNeval_ast(ast%, env%)
  IF FNis_empty(ast%) THEN =ast%
  car% = FNfirst(ast%)
  IF FNis_symbol(car%) THEN
    CASE FNunbox_symbol(car%) OF
      REM  Special forms
      WHEN "def!"
        LOCAL val%
	val% = FNEVAL(FNnth(ast%, 2), env%)
        PROCenv_set(env%, FNnth(ast%, 1), val%)
	=val%
      WHEN "let*"
        LOCAL bindings%
	env% = FNalloc_environment(env%)
	bindings% = FNnth(ast%, 1)
	WHILE NOT FNis_empty(bindings%)
	  PROCenv_set(env%, FNfirst(bindings%), FNEVAL(FNnth(bindings%, 1), env%))
	  bindings% = FNrest(FNrest(bindings%))
	ENDWHILE
	=FNEVAL(FNnth(ast%, 2), env%)
      WHEN "do"
        LOCAL val%
        ast% = FNeval_ast(FNrest(ast%), env%)
	REPEAT
	  val% = FNfirst(ast%)
	  ast% = FNrest(ast%)
	UNTIL FNis_empty(ast%)
	=val%
      WHEN "if"
        IF FNis_truish(FNEVAL(FNnth(ast%, 1), env%)) THEN
	  =FNEVAL(FNnth(ast%, 2), env%)
	ENDIF
	IF FNcount(ast%) = 3 THEN =FNnil
	=FNEVAL(FNnth(ast%, 3), env%)
      WHEN "fn*"
	=FNalloc_fn(FNnth(ast%, 2), FNnth(ast%, 1), env%)
    ENDCASE
  ENDIF
  ast% = FNeval_ast(ast%, env%)
  car% = FNfirst(ast%)
  IF FNis_fn(car%) THEN
    env% = FNnew_env(FNfn_env(car%), FNfn_params(car%), FNrest(ast%))
    =FNEVAL(FNfn_ast(car%), env%)
  ENDIF
  IF FNis_corefn(car%) THEN
    =FNcore_call(FNunbox_corefn(car%), FNrest(ast%))
  ENDIF
  ERROR &40E80918, "Not a function"

DEF FNPRINT(a%)
=FNunbox_string(FNpr_str(a%, TRUE))

DEF FNrep(a$)
=FNPRINT(FNEVAL(FNREAD(a$), repl_env%))

DEF FNeval_ast(ast%, env%)
  LOCAL val%, car%, cdr%, map%, keys%, key$
  IF FNis_symbol(ast%) THEN =FNenv_get(env%, ast%)
  IF FNis_seq(ast%) THEN
    IF FNis_empty(ast%) THEN =ast%
    car% = FNEVAL(FNfirst(ast%), env%)
    cdr% = FNeval_ast(FNrest(ast%), env%)
    IF FNis_vector(ast%) THEN =FNalloc_vector_pair(car%, cdr%)
    =FNalloc_pair(car%, cdr%)
  ENDIF
  IF FNis_hashmap(ast%) THEN
    map% = FNempty_hashmap
    keys% = FNhashmap_keys(ast%)
    WHILE NOT FNis_empty(keys%)
      key$ = FNunbox_string(FNfirst(keys%))
      map% = FNhashmap_set(map%, key$, FNEVAL(FNhashmap_get(ast%, key$), env%))
      keys% = FNrest(keys%)
    ENDWHILE
    =map%
  ENDIF
=ast%

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
