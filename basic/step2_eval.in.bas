GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

REM READ(A$) -> R%
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL_AST(A%, E%) -> R%
EVAL_AST:
  ZL%=ZL%+2: ZZ%(ZL%-1)=E%: ZZ%(ZL%)=A%
  IF ER%=1 THEN GOTO EVAL_AST_RETURN

  REM AZ%=A%: GOSUB PR_STR
  REM PRINT "EVAL_AST: " + R$ + "(" + STR$(R%) + ")"

  T%=Z%(A%,0)
  IF T%=5 THEN EVAL_AST_SYMBOL
  IF T%=6 THEN EVAL_AST_LIST
  R%=A%
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    HM%=E%: K%=A%: GOSUB HASHMAP_GET
    IF T3%=0 THEN ER%=1: ER$="'" + ZS$(Z%(A%,1)) + "' not found"
    GOTO EVAL_AST_RETURN
  
  EVAL_AST_LIST:
    REM push future return value (new list)
    ZL%=ZL%+1
    ZZ%(ZL%)=ZI%
    REM push previous new list entry
    ZL%=ZL%+1
    ZZ%(ZL%)=ZI%

    EVAL_AST_LIST_LOOP:
      REM create new list entry
      Z%(ZI%,0)=6
      Z%(ZI%,1)=0
      ZI%=ZI%+1

      REM check if we are done evaluating the list
      IF Z%(A%,1)=0 THEN GOTO EVAL_AST_LIST_LOOP_DONE

      REM create value ptr placeholder
      Z%(ZI%,0)=15
      Z%(ZI%,1)=0
      ZI%=ZI%+1

      REM call EVAL for each entry
      A%=A%+1: GOSUB EVAL
      A%=A%-1

      REM update previous list entry to point to current entry
      Z%(ZZ%(ZL%),1)=ZI%
      REM update previous value pointer to evaluated entry
      Z%(ZZ%(ZL%)+1,1)=R%
      REM update previous ptr to current entry
      ZZ%(ZL%)=ZI%

      REM process the next list entry
      A%=Z%(A%,1)

      GOTO EVAL_AST_LIST_LOOP
    EVAL_AST_LIST_LOOP_DONE:
      REM pop previous new list entry value
      ZL%=ZL%-1
      REM pop return value (new list)
      R%=ZZ%(ZL%)
      ZL%=ZL%-1
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    E%=ZZ%(ZL%-1): A%=ZZ%(ZL%): ZL%=ZL%-2
    RETURN

REM EVAL(A%, E%)) -> R%
EVAL:
  ZL%=ZL%+2: ZZ%(ZL%-1)=E%: ZZ%(ZL%)=A%
  IF ER%=1 THEN GOTO EVAL_RETURN

  REM AZ%=A%: GOSUB PR_STR
  REM PRINT "EVAL: " + R$ + "(" + STR$(R%) + ")"

  GOSUB LIST_Q
  IF R% THEN GOTO APPLY_LIST
  REM ELSE
    GOSUB EVAL_AST
    GOTO EVAL_RETURN

  APPLY_LIST:
    GOSUB EMPTY_Q
    IF R% THEN R%=A%: GOTO EVAL_RETURN

    GOSUB EVAL_AST
    IF ER%=1 THEN GOTO EVAL_RETURN
    F%=R%+1
    AR%=Z%(R%,1): REM REST
    R%=F%: GOSUB DEREF
    F%=R%
    IF Z%(F%,0)<>12 THEN ER%=1: ER$="apply of non-function": GOTO EVAL_RETURN
    GOSUB DO_FUNCTION

    GOTO EVAL_RETURN

  EVAL_RETURN:
    E%=ZZ%(ZL%-1): A%=ZZ%(ZL%): ZL%=ZL%-2
    RETURN

REM DO_FUNCTION(F%, AR%)
DO_FUNCTION:
  AZ%=F%: GOSUB PR_STR
  F$=R$
  AZ%=AR%: GOSUB PR_STR
  AR$=R$

  REM Get the function number
  FF%=Z%(F%,1)

  REM Get argument values
  R%=AR%+1: GOSUB DEREF
  AA%=Z%(R%,1)
  R%=Z%(AR%,1)+1: GOSUB DEREF
  AB%=Z%(R%,1)

  REM Allocate the return value
  R%=ZI%
  ZI%=ZI%+1

  REM Switch on the function number
  IF FF%=1 THEN DO_ADD
  IF FF%=2 THEN DO_SUB
  IF FF%=3 THEN DO_MULT
  IF FF%=4 THEN DO_DIV
  ER%=1: ER$="unknown function" + STR$(FF%): RETURN

  DO_ADD:
    Z%(R%,0)=2
    Z%(R%,1)=AA%+AB%
    GOTO DO_FUNCTION_DONE
  DO_SUB:
    Z%(R%,0)=2
    Z%(R%,1)=AA%-AB%
    GOTO DO_FUNCTION_DONE
  DO_MULT:
    Z%(R%,0)=2
    Z%(R%,1)=AA%*AB%
    GOTO DO_FUNCTION_DONE
  DO_DIV:
    Z%(R%,0)=2
    Z%(R%,1)=AA%/AB%
    GOTO DO_FUNCTION_DONE

  DO_FUNCTION_DONE:
    RETURN

REM PRINT(A%) -> R$
MAL_PRINT:
  AZ%=A%: GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
REM Assume RE% has repl_env
REP:
  GOSUB MAL_READ
  IF ER% THEN RETURN
  A%=R%: E%=RE%: GOSUB EVAL
  IF ER% THEN RETURN
  A%=R%: GOSUB MAL_PRINT
  IF ER% THEN RETURN
  RETURN

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  REM repl_env
  GOSUB HASHMAP
  RE%=R%

  REM + function
  A%=1: GOSUB NATIVE_FUNCTION
  HM%=RE%: K$="+": V%=R%: GOSUB ASSOC1_S
  RE%=R%

  REM - function
  A%=2: GOSUB NATIVE_FUNCTION
  HM%=RE%: K$="-": V%=R%: GOSUB ASSOC1_S
  RE%=R%

  REM * function
  A%=3: GOSUB NATIVE_FUNCTION
  HM%=RE%: K$="*": V%=R%: GOSUB ASSOC1_S
  RE%=R%

  REM / function
  A%=4: GOSUB NATIVE_FUNCTION
  HM%=RE%: K$="/": V%=R%: GOSUB ASSOC1_S
  RE%=R%

  AZ%=RE%: GOSUB PR_STR
  PRINT "env: " + R$ + "(" + STR$(RE%) + ")"

  MAIN_LOOP:
    A$="user> "
    GOSUB READLINE: REM /* call input parser */
    IF EOF=1 THEN GOTO MAIN_DONE
    A$=R$: GOSUB REP: REM /* call REP */
    IF ER% THEN GOTO ERROR
    PRINT R$
    GOTO MAIN_LOOP

    ERROR:
      PRINT "Error: " + ER$
      ER%=0
      ER$=""
      GOTO MAIN_LOOP

  MAIN_DONE:
    PRINT "Free: " + STR$(FRE(0)): REM abc
    END

