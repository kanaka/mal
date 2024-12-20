REM Step 8 of mal in BBC BASIC

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

REM  Initial forms to evaluate
RESTORE +0
DATA (def! not (fn* (a) (if a false true)))
DATA (def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
DATA (defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))
DATA ""
REPEAT
  READ form$
  IF form$ <> "" THEN val$ = FNrep(form$)
UNTIL form$ = ""

argv% = FNget_argv

IF FNis_empty(argv%) THEN
  PROCenv_set(repl_env%, "*ARGV*", FNempty)
ELSE
  PROCenv_set(repl_env%, "*ARGV*", FNrest(argv%))
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

DEF FNstarts_with(ast%, sym$)
  LOCAL a0%
  IF NOT FNis_list(ast%) THEN =FALSE
  a0% = FNfirst(ast%)
  IF NOT FNis_symbol(a0%) THEN =FALSE
  =FNunbox_symbol(a0%) = sym$

DEF FNqq_elts(seq%)
  LOCAL elt%, acc%
  IF FNis_empty(seq%) THEN =FNempty
  elt% = FNfirst(seq%)
  acc% = FNqq_elts(FNrest(seq%))
  IF FNstarts_with(elt%, "splice-unquote") THEN
    =FNalloc_list3(FNalloc_symbol("concat"), FNnth(elt%, 1), acc%)
  ENDIF
  =FNalloc_list3(FNalloc_symbol("cons"), FNquasiquote(elt%), acc%)

DEF FNquasiquote(ast%)
  IF FNstarts_with(ast%, "unquote") THEN =FNnth(ast%, 1)
  IF FNis_list(ast%) THEN =FNqq_elts(ast%)
  IF FNis_vector(ast%) THEN
    =FNalloc_list2(FNalloc_symbol("vec"), FNqq_elts(ast%))
  ENDIF
  IF FNis_symbol(ast%) OR FNis_hashmap(ast%) THEN
    =FNalloc_list2(FNalloc_symbol("quote"), ast%)
  ENDIF
  =ast%

DEF FNEVAL(ast%, env%)
  PROCgc_enter
=FNgc_exit(FNEVAL_(ast%, env%))

DEF FNEVAL_(ast%, env%)
    LOCAL car%, val%, bindings%, key$
31416 REM tail call optimization loop
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
        WHEN "defmacro!"
          val% = FNEVAL(FNnth(ast%, 2), env%)
          IF FNis_fn(val%) THEN val% = FNas_macro(val%)
          PROCenv_set(env%, FNunbox_symbol(FNnth(ast%, 1)), val%)
          =val%
        WHEN "let*"
          env% = FNalloc_environment(env%)
          bindings% = FNnth(ast%, 1)
          WHILE NOT FNis_empty(bindings%)
            PROCenv_set(env%, FNunbox_symbol(FNfirst(bindings%)), FNEVAL(FNnth(bindings%, 1), env%))
            bindings% = FNrest(FNrest(bindings%))
          ENDWHILE
          ast% = FNnth(ast%, 2)
          GOTO 31416
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
          GOTO 31416
        WHEN "if"
          IF FNis_truish(FNEVAL(FNnth(ast%, 1), env%)) THEN
            ast% = FNnth(ast%, 2)
          ELSE
            IF FNcount(ast%) = 3 THEN =FNnil
            ast% = FNnth(ast%, 3)
          ENDIF
          GOTO 31416
        WHEN "fn*"
          =FNalloc_fn(FNnth(ast%, 2), FNnth(ast%, 1), env%)
        WHEN "quote"
          =FNnth(ast%, 1)
        WHEN "quasiquote"
          ast% = FNquasiquote(FNnth(ast%, 1))
          GOTO 31416
        OTHERWISE
          car% = FNenv_get(env%, key$)
      ENDCASE
    ELSE
      car% = FNEVAL(car%, env%)
    ENDIF
    REM  This is the "apply" part.
    ast% = FNrest(ast%)
    IF FNis_macro(car%) THEN
      ast% = FNEVAL(FNfn_ast(car%), FNnew_env(FNfn_env(car%), FNfn_params(car%), ast%))
      GOTO 31416
    ENDIF
    ast% = FNeval_ast(ast%, env%)
    IF FNis_corefn(car%) THEN
      =FNcore_call(FNunbox_corefn(car%), ast%)
    ENDIF
    IF FNis_fn(car%) THEN
      env% = FNnew_env(FNfn_env(car%), FNfn_params(car%), ast%)
      ast% = FNfn_ast(car%)
      GOTO 31416
    ENDIF
    ERROR &40E80918, "Not a function"

DEF FNPRINT(a%)
=FNunbox_string(FNpr_str(a%, TRUE))

DEF FNrep(a$)
=FNPRINT(FNEVAL(FNREAD(a$), repl_env%))

DEF FNeval_ast(ast%, env%)
    IF FNis_empty(ast%) THEN =ast%
    =FNalloc_pair(FNEVAL(FNfirst(ast%), env%), FNeval_ast(FNrest(ast%), env%))

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
