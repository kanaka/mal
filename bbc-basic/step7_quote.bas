REM Step 7 of mal in BBC BASIC

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

REM  Initial forms to evaluate
RESTORE +0
DATA (def! not (fn* (a) (if a false true)))
DATA (def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
DATA ""
REPEAT
  READ form$
  IF form$ <> "" THEN val$ = FNrep(form$)
UNTIL form$ = ""

argv% = FNget_argv

IF FNis_empty(argv%) THEN
  PROCenv_set(repl_env%, FNalloc_symbol("*ARGV*"), FNempty)
ELSE
  PROCenv_set(repl_env%, FNalloc_symbol("*ARGV*"), FNrest(argv%))
  val$ = FNrep("(load-file " + FNunbox_string(FNpr_str(FNfirst(argv%), TRUE)) + ")")
  END
ENDIF

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

DEF FNis_pair(val%)
=FNis_seq(val%) AND NOT FNis_empty(val%)

DEF FNquasiquote(ast%)
  LOCAL car%, caar%
  IF NOT FNis_pair(ast%) THEN =FNalloc_list2(FNalloc_symbol("quote"), ast%)
  car% = FNfirst(ast%)
  IF FNis_symbol(car%) THEN
    IF FNunbox_symbol(car%) = "unquote" THEN =FNnth(ast%, 1)
  ENDIF
  IF FNis_pair(car%) THEN
    caar% = FNfirst(car%)
    IF FNis_symbol(caar%) THEN
      IF FNunbox_symbol(caar%) = "splice-unquote" THEN
        =FNalloc_list3(FNalloc_symbol("concat"), FNnth(car%, 1), FNquasiquote(FNrest(ast%)))
      ENDIF
    ENDIF
  ENDIF
=FNalloc_list3(FNalloc_symbol("cons"), FNquasiquote(car%), FNquasiquote(FNrest(ast%)))

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
        WHEN "quote"
          =FNnth(ast%, 1)
        WHEN "quasiquote"
          ast% = FNquasiquote(FNnth(ast%, 1))
          REM  Loop round for tail-call optimisation
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

DEF FNget_argv
  PROCgc_enter
  LOCAL argv%, rargv%, cmdptr%, arg$, len%
  argv% = FNempty
  IF !PAGE = &D7C1C7C5 THEN
    REM  Running under Brandy, so ARGC and ARGV$ are usable.
    IF ARGC >= 1 THEN
      FOR i% = ARGC TO 1 STEP -1
        argv% = FNalloc_pair(FNalloc_string(ARGV$(i%)), argv%)
      NEXT i%
    ENDIF
  ELSE
    IF (INKEY(-256) AND &F0) = &A0 THEN
      rargv% = FNempty
      REM  Running under RISC OS
      REM  Vexingly, we can only get the command line that was passed to
      REM  the BASIC interpreter.  This means that we need to extract
      REM  the arguments from that.  Typically, we will have been started
      REM  with "BASIC -quit <filename> <args>".

      DIM q% 256
      SYS "OS_GetEnv" TO cmdptr%
      WHILE ?cmdptr% >= 32
        SYS "OS_GSTrans", cmdptr%, q%, &20000000 + 256 TO cmdptr%, , len%
        q%?len% = 13
        rargv% = FNalloc_pair(FNalloc_string($q%), rargv%)
      ENDWHILE
      REM  Put argv back into the right order.
      WHILE NOT FNis_empty(rargv%)
        argv% = FNalloc_pair(FNfirst(rargv%), argv%)
        rargv% = FNrest(rargv%)
      ENDWHILE
      IF FNis_empty(argv%) THEN =FNgc_exit(argv%)
      argv% = FNrest(argv%) : REM skip "BASIC"
      IF FNis_empty(argv%) THEN =FNgc_exit(argv%)
      IF FNunbox_string(FNfirst(argv%)) <> "-quit" THEN =FNgc_exit(argv%)
      argv% = FNrest(argv%) : REM skip "-quit"
      IF FNis_empty(argv%) THEN =FNgc_exit(argv%)
      argv% = FNrest(argv%) : REM skip filename
    ENDIF
  ENDIF
=FNgc_exit(argv%)


REM Local Variables:
REM indent-tabs-mode: nil
REM End:
