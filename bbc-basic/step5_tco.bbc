REM Step 5 of mal in BBC BASIC

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
  PROCgc_enter
=FNgc_exit(FNEVAL_(ast%, env%))

DEF FNEVAL_(ast%, env%)
  LOCAL car%, specialform%, val%, bindings%
  REPEAT
    PROCgc_keep_only2(ast%, env%)
    IF NOT FNis_list(ast%) THEN =FNeval_ast(ast%, env%)
    IF FNis_empty(ast%) THEN =ast%
    car% = FNfirst(ast%)
    specialform% = FALSE
    IF FNis_symbol(car%) THEN
      specialform% = TRUE
      CASE FNunbox_symbol(car%) OF
        REM  Special forms
        WHEN "def!"
          val% = FNEVAL(FNnth(ast%, 2), env%)
          PROCenv_set(env%, FNnth(ast%, 1), val%)
          =val%
        WHEN "let*"
          env% = FNalloc_environment(env%)
          bindings% = FNnth(ast%, 1)
          WHILE NOT FNis_empty(bindings%)
            PROCenv_set(env%, FNfirst(bindings%), FNEVAL(FNnth(bindings%, 1), env%))
            bindings% = FNrest(FNrest(bindings%))
          ENDWHILE
          ast% = FNnth(ast%, 2)
          REM  Loop round for tail-call optimisation.
        WHEN "do"
          REM  The guide has us call FNeval_ast on the sub-list that excludes
          REM  the last element of ast%, but that's a bit painful without
          REM  native list slicing, so it's easier to just re-implement the
          REM  bit of FNeval_ast that we need.
          ast% = FNrest(ast%)
          WHILE NOT FNis_empty(FNrest(ast%))
            val% = FNEVAL(FNfirst(ast%), env%)
            ast% = FNrest(ast%)
          ENDWHILE
          ast% = FNfirst(ast%)
        WHEN "if"
          IF FNis_truish(FNEVAL(FNnth(ast%, 1), env%)) THEN
            ast% = FNnth(ast%, 2)
          ELSE
            IF FNcount(ast%) = 3 THEN =FNnil ELSE ast% = FNnth(ast%, 3)
          ENDIF
          REM  Loop round for tail-call optimisation.
        WHEN "fn*"
          =FNalloc_fn(FNnth(ast%, 2), FNnth(ast%, 1), env%)
        OTHERWISE
          specialform% = FALSE
      ENDCASE
    ENDIF
    IF NOT specialform% THEN
      REM  This is the "apply" part.
      ast% = FNeval_ast(ast%, env%)
      car% = FNfirst(ast%)
      IF FNis_corefn(car%) THEN
        =FNcore_call(FNunbox_corefn(car%), FNrest(ast%))
      ENDIF
      IF FNis_fn(car%) THEN
        env% = FNnew_env(FNfn_env(car%), FNfn_params(car%), FNrest(ast%))
        ast% = FNfn_ast(car%)
        REM  Loop round for tail-call optimisation.
      ELSE
        ERROR &40E80918, "Not a function"
      ENDIF
    ENDIF
  UNTIL FALSE

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
