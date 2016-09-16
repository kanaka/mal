GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'
REM $INCLUDE: 'env.in.bas'

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
  IF T%=6 THEN EVAL_AST_SEQ
  IF T%=7 THEN EVAL_AST_SEQ
  IF T%=8 THEN EVAL_AST_SEQ
  R%=A%
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    K%=A%: GOSUB ENV_GET
    GOTO EVAL_AST_RETURN
  
  EVAL_AST_SEQ:
    REM push type of sequence
    ZL%=ZL%+1
    ZZ%(ZL%)=T%
    REM push sequence index
    ZL%=ZL%+1
    ZZ%(ZL%)=-1
    REM push future return value (new sequence)
    ZL%=ZL%+1
    ZZ%(ZL%)=ZI%
    REM push previous new sequence entry
    ZL%=ZL%+1
    ZZ%(ZL%)=ZI%

    EVAL_AST_SEQ_LOOP:
      REM create new sequence entry
      Z%(ZI%,0)=ZZ%(ZL%-3)
      Z%(ZI%,1)=0
      ZI%=ZI%+1

      REM update index
      ZZ%(ZL%-2)=ZZ%(ZL%-2)+1

      REM check if we are done evaluating the sequence
      IF Z%(A%,1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM create value ptr placeholder
      Z%(ZI%,0)=15
      Z%(ZI%,1)=0
      ZI%=ZI%+1

      REM if hashmap, skip eval of even entries (keys)
      R%=A%+1
      IF (ZZ%(ZL%-3)=8) AND ((ZZ%(ZL%-2) AND 1)=0) THEN GOTO EVAL_AST_SEQ_SKIP

      REM call EVAL for each entry
      A%=A%+1: GOSUB EVAL
      A%=A%-1

      EVAL_AST_SEQ_SKIP:

      REM update previous sequence entry to point to current entry
      Z%(ZZ%(ZL%),1)=ZI%
      REM update previous value pointer to evaluated entry
      Z%(ZZ%(ZL%)+1,1)=R%
      REM update previous ptr to current entry
      ZZ%(ZL%)=ZI%

      REM process the next sequence entry
      A%=Z%(A%,1)

      GOTO EVAL_AST_SEQ_LOOP
    EVAL_AST_SEQ_LOOP_DONE:
      REM pop previous new sequence entry value
      ZL%=ZL%-1
      REM pop return value (new seq), index, and seq type
      R%=ZZ%(ZL%)
      ZL%=ZL%-3
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

    A0% = A%+1
    R%=A0%: GOSUB DEREF
    A0%=R%

    REM get symbol in A$
    IF Z%(A0%,0)<>5 THEN A$=""
    IF Z%(A0%,0)=5 THEN A$=ZS$(Z%(A0%,1))

    IF A$="def!" THEN GOTO EVAL_DEF
    IF A$="let*" THEN GOTO EVAL_LET
    GOTO EVAL_INVOKE

    EVAL_GET_A3:
      A3% = Z%(Z%(Z%(A%,1),1),1)+1
      R%=A3%: GOSUB DEREF
      A3%=R%
    EVAL_GET_A2:
      A2% = Z%(Z%(A%,1),1)+1
      R%=A2%: GOSUB DEREF
      A2%=R%
    EVAL_GET_A1:
      A1% = Z%(A%,1)+1
      R%=A1%: GOSUB DEREF
      A1%=R%
      RETURN

    EVAL_DEF:
      REM PRINT "def!"
      GOSUB EVAL_GET_A2: REM set a1% and a2%
      A%=A2%: GOSUB EVAL: REM eval a2
      K%=A1%: V%=R%: GOSUB ENV_SET: REM set a1 in env to a2
      RETURN
    EVAL_LET:
      GOSUB EVAL_GET_A2: REM set a1% and a2%
      REM create new environment with outer as current environment
      EO%=E%: GOSUB ENV_NEW
      E%=R%
      EVAL_LET_LOOP:
        IF Z%(A1%,1)=0 THEN GOTO EVAL_LET_LOOP_DONE
        REM push A1%
        ZL%=ZL%+1: ZZ%(ZL%)=A1%
        REM eval current A1 odd element
        A%=Z%(A1%,1)+1: GOSUB EVAL
        REM pop A1%
        A1%=ZZ%(ZL%): ZL%=ZL%-1
        REM set environment: even A1% key to odd A1% eval'd above
        K%=A1%+1: V%=R%: GOSUB ENV_SET
        REM skip to the next pair of A1% elements
        A1%=Z%(Z%(A1%,1),1)
        GOTO EVAL_LET_LOOP
      EVAL_LET_LOOP_DONE:
        A%=A2%: GOSUB EVAL: REM eval a2 using let_env
        RETURN
    EVAL_INVOKE:
      GOSUB EVAL_AST
      IF ER%=1 THEN GOTO EVAL_RETURN
      F%=R%+1
      AR%=Z%(R%,1): REM REST
      R%=F%: GOSUB DEREF
      F%=R%
      IF Z%(F%,0)<>9 THEN ER%=1: ER$="apply of non-function": GOTO EVAL_RETURN
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
  AZ%=A%: PR%=1: GOSUB PR_STR
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
  EO%=-1: GOSUB ENV_NEW
  RE%=R%

  E%=RE%
  REM + function
  A%=1: GOSUB NATIVE_FUNCTION
  K$="+": V%=R%: GOSUB ENV_SET_S

  REM - function
  A%=2: GOSUB NATIVE_FUNCTION
  K$="-": V%=R%: GOSUB ENV_SET_S

  REM * function
  A%=3: GOSUB NATIVE_FUNCTION
  K$="*": V%=R%: GOSUB ENV_SET_S

  REM / function
  A%=4: GOSUB NATIVE_FUNCTION
  K$="/": V%=R%: GOSUB ENV_SET_S

  AZ%=Z%(RE%,1): GOSUB PR_STR
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
    GOSUB PR_MEMORY_SUMMARY
    END

