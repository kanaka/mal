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
    PROCenv_set(repl_env%, sym$, FNalloc_corefn(i%))
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
  LOCAL car%, val%, bindings%, key$
  PROCgc_keep_only2(ast%, env%)
  val% = FNenv_find(env%, "DEBUG-EVAL")
  IF NOT FNis_nil(val%) THEN
    IF FNis_truish(FNenv_get(val%, "DEBUG-EVAL")) THEN
      PRINT "EVAL: " + FNunbox_string(FNpr_str(ast%, TRUE))
    ENDIF
  ENDIF
  IF FNis_symbol(ast%) THEN =FNenv_get(env%, FNunbox_symbol(ast%))
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
  car% = FNfirst(ast%)
  IF FNis_vector(ast%) THEN =FNalloc_vector_pair(FNEVAL(car%, env%), FNeval_ast(FNrest(ast%), env%))
  IF FNis_symbol(car%) THEN
    key$ = FNunbox_symbol(car%)
    CASE key$ OF
      REM  Special forms
      WHEN "def!"
        val% = FNEVAL(FNnth(ast%, 2), env%)
        PROCenv_set(env%, FNunbox_symbol(FNnth(ast%, 1)), val%)
        =val%
      WHEN "let*"
        env% = FNalloc_environment(env%)
        bindings% = FNnth(ast%, 1)
        WHILE NOT FNis_empty(bindings%)
          PROCenv_set(env%, FNunbox_symbol(FNfirst(bindings%)), FNEVAL(FNnth(bindings%, 1), env%))
          bindings% = FNrest(FNrest(bindings%))
        ENDWHILE
        =FNEVAL(FNnth(ast%, 2), env%)
      WHEN "do"
        WHILE TRUE
          ast% = FNrest(ast%)
          IF FNis_empty(ast%) THEN = val%
          val% = FNEVAL(FNfirst(ast%), env%)
        ENDWHILE
      WHEN "if"
        IF FNis_truish(FNEVAL(FNnth(ast%, 1), env%)) THEN =FNEVAL(FNnth(ast%, 2), env%)
        IF FNcount(ast%) = 3 THEN =FNnil
        =FNEVAL(FNnth(ast%, 3), env%)
      WHEN "fn*"
        =FNalloc_fn(FNnth(ast%, 2), FNnth(ast%, 1), env%)
      OTHERWISE
        car% = FNenv_get(env%, key$)
    ENDCASE
  ELSE
    car% = FNEVAL(car%, env%)
  ENDIF
  REM  This is the "apply" part.
  ast% = FNeval_ast(FNrest(ast%), env%)
  IF FNis_corefn(car%) THEN
    =FNcore_call(FNunbox_corefn(car%), ast%)
  ENDIF
  IF FNis_fn(car%) THEN
    env% = FNnew_env(FNfn_env(car%), FNfn_params(car%), ast%)
    =FNEVAL(FNfn_ast(car%), env%)
  ENDIF
  ERROR &40E80918, "Not a function"

DEF FNPRINT(a%)
=FNunbox_string(FNpr_str(a%, TRUE))

DEF FNrep(a$)
=FNPRINT(FNEVAL(FNREAD(a$), repl_env%))

DEF FNeval_ast(ast%, env%)
    IF FNis_empty(ast%) THEN =ast%
    =FNalloc_pair(FNEVAL(FNfirst(ast%), env%), FNeval_ast(FNrest(ast%), env%))

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
