GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ(A$) -> R%
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL_AST(A%, E%) -> R%
EVAL_AST:
  LV%=LV%+1

  REM push A% and E% on the stack
  ZL%=ZL%+2:ZZ%(ZL%-1)=E%:ZZ%(ZL%)=A%

  IF ER%<>-2 THEN GOTO EVAL_AST_RETURN

  GOSUB DEREF_A

  T%=Z%(A%,0)AND15
  IF T%=5 THEN GOTO EVAL_AST_SYMBOL
  IF T%>=6 AND T%<=8 THEN GOTO EVAL_AST_SEQ

  REM scalar: deref to actual value and inc ref cnt
  R%=A%:GOSUB DEREF_R
  Z%(R%,0)=Z%(R%,0)+16
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    HM%=E%:K%=A%:GOSUB HASHMAP_GET
    GOSUB DEREF_R
    IF T3%=0 THEN ER%=-1:ER$="'"+ZS$(Z%(A%,1))+"' not found":GOTO EVAL_AST_RETURN
    Z%(R%,0)=Z%(R%,0)+16
    GOTO EVAL_AST_RETURN

  EVAL_AST_SEQ:
    REM allocate the first entry
    SZ%=2:GOSUB ALLOC

    REM make space on the stack
    ZL%=ZL%+4
    REM push type of sequence
    ZZ%(ZL%-3)=T%
    REM push sequence index
    ZZ%(ZL%-2)=-1
    REM push future return value (new sequence)
    ZZ%(ZL%-1)=R%
    REM push previous new sequence entry
    ZZ%(ZL%)=R%

    EVAL_AST_SEQ_LOOP:
      REM set new sequence entry type (with 1 ref cnt)
      Z%(R%,0)=ZZ%(ZL%-3)+16
      Z%(R%,1)=0
      REM create value ptr placeholder
      Z%(R%+1,0)=14
      Z%(R%+1,1)=0

      REM update index
      ZZ%(ZL%-2)=ZZ%(ZL%-2)+1

      REM check if we are done evaluating the source sequence
      IF Z%(A%,1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM if hashmap, skip eval of even entries (keys)
      IF (ZZ%(ZL%-3)=8) AND ((ZZ%(ZL%-2) AND 1)=0) THEN GOTO EVAL_AST_DO_REF
      GOTO EVAL_AST_DO_EVAL

      EVAL_AST_DO_REF:
        R%=A%+1:GOSUB DEREF_R: REM deref to target of referred entry
        Z%(R%,0)=Z%(R%,0)+16: REM inc ref cnt of referred value
        GOTO EVAL_AST_ADD_VALUE

      EVAL_AST_DO_EVAL:
        REM call EVAL for each entry
        A%=A%+1:GOSUB EVAL
        A%=A%-1
        GOSUB DEREF_R: REM deref to target of evaluated entry

      EVAL_AST_ADD_VALUE:

      REM update previous value pointer to evaluated entry
      Z%(ZZ%(ZL%)+1,1)=R%

      IF ER%<>-2 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM allocate the next entry
      SZ%=2:GOSUB ALLOC

      REM update previous sequence entry value to point to new entry
      Z%(ZZ%(ZL%),1)=R%
      REM update previous ptr to current entry
      ZZ%(ZL%)=R%

      REM process the next sequence entry from source list
      A%=Z%(A%,1)

      GOTO EVAL_AST_SEQ_LOOP
    EVAL_AST_SEQ_LOOP_DONE:
      REM get return value (new seq), index, and seq type
      R%=ZZ%(ZL%-1)
      REM pop previous, return, index and type
      ZL%=ZL%-4
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    REM pop A% and E% off the stack
    E%=ZZ%(ZL%-1):A%=ZZ%(ZL%):ZL%=ZL%-2

    LV%=LV%-1
    RETURN

REM EVAL(A%, E%)) -> R%
EVAL:
  LV%=LV%+1: REM track basic return stack level

  REM push A% and E% on the stack
  ZL%=ZL%+2:ZZ%(ZL%-1)=E%:ZZ%(ZL%)=A%

  REM AZ%=A%:PR%=1:GOSUB PR_STR
  REM PRINT "EVAL: "+R$+" [A%:"+STR$(A%)+", LV%:"+STR$(LV%)+"]"

  GOSUB DEREF_A

  GOSUB LIST_Q
  IF R% THEN GOTO APPLY_LIST
  REM ELSE
    GOSUB EVAL_AST
    GOTO EVAL_RETURN

  APPLY_LIST:
    GOSUB EMPTY_Q
    IF R% THEN R%=A%:Z%(R%,0)=Z%(R%,0)+16:GOTO EVAL_RETURN

    EVAL_INVOKE:
      GOSUB EVAL_AST
      R3%=R%

      REM if error, return f/args for release by caller
      IF ER%<>-2 THEN GOTO EVAL_RETURN
      F%=R%+1

      AR%=Z%(R%,1): REM rest
      R%=F%:GOSUB DEREF_R:F%=R%
      IF (Z%(F%,0)AND15)<>9 THEN ER%=-1:ER$="apply of non-function":GOTO EVAL_RETURN
      GOSUB DO_FUNCTION
      AY%=R3%:GOSUB RELEASE
      GOTO EVAL_RETURN

  EVAL_RETURN:

    LV%=LV%-1: REM track basic return stack level


    REM trigger GC
    TA%=FRE(0)

    REM pop A% and E% off the stack
    E%=ZZ%(ZL%-1):A%=ZZ%(ZL%):ZL%=ZL%-2

    RETURN

REM DO_FUNCTION(F%, AR%)
DO_FUNCTION:
  AZ%=F%:GOSUB PR_STR
  F$=R$
  AZ%=AR%:GOSUB PR_STR
  AR$=R$

  REM Get the function number
  FF%=Z%(F%,1)

  REM Get argument values
  R%=AR%+1:GOSUB DEREF_R:AA%=Z%(R%,1)
  R%=Z%(AR%,1)+1:GOSUB DEREF_R:AB%=Z%(R%,1)

  REM Allocate the return value
  SZ%=1:GOSUB ALLOC

  REM Switch on the function number
  IF FF%=1 THEN GOTO DO_ADD
  IF FF%=2 THEN GOTO DO_SUB
  IF FF%=3 THEN GOTO DO_MULT
  IF FF%=4 THEN GOTO DO_DIV
  ER%=-1:ER$="unknown function"+STR$(FF%):RETURN

  DO_ADD:
    Z%(R%,0)=2+16
    Z%(R%,1)=AA%+AB%
    GOTO DO_FUNCTION_DONE
  DO_SUB:
    Z%(R%,0)=2+16
    Z%(R%,1)=AA%-AB%
    GOTO DO_FUNCTION_DONE
  DO_MULT:
    Z%(R%,0)=2+16
    Z%(R%,1)=AA%*AB%
    GOTO DO_FUNCTION_DONE
  DO_DIV:
    Z%(R%,0)=2+16
    Z%(R%,1)=AA%/AB%
    GOTO DO_FUNCTION_DONE

  DO_FUNCTION_DONE:
    RETURN

REM PRINT(A%) -> R$
MAL_PRINT:
  AZ%=A%:PR%=1:GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
REM Assume RE% has repl_env
REP:
  R1%=0:R2%=0
  GOSUB MAL_READ
  R1%=R%
  IF ER%<>-2 THEN GOTO REP_DONE

  A%=R%:E%=RE%:GOSUB EVAL
  R2%=R%
  IF ER%<>-2 THEN GOTO REP_DONE

  A%=R%:GOSUB MAL_PRINT
  RT$=R$

  REP_DONE:
    REM Release memory from MAL_READ and EVAL
    IF R2%<>0 THEN AY%=R2%:GOSUB RELEASE
    IF R1%<>0 THEN AY%=R1%:GOSUB RELEASE
    R$=RT$
    RETURN

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  LV%=0

  REM create repl_env
  GOSUB HASHMAP:RE%=R%

  REM + function
  A%=1:GOSUB NATIVE_FUNCTION
  HM%=RE%:K$="+":V%=R%:GOSUB ASSOC1_S:RE%=R%

  REM - function
  A%=2:GOSUB NATIVE_FUNCTION
  HM%=RE%:K$="-":V%=R%:GOSUB ASSOC1_S:RE%=R%

  REM * function
  A%=3:GOSUB NATIVE_FUNCTION
  HM%=RE%:K$="*":V%=R%:GOSUB ASSOC1_S:RE%=R%

  REM / function
  A%=4:GOSUB NATIVE_FUNCTION
  HM%=RE%:K$="/":V%=R%:GOSUB ASSOC1_S:RE%=R%

  ZT%=ZI%: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EOF=1 THEN GOTO QUIT

    A$=R$:GOSUB REP: REM call REP

    IF ER%<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM P1%=ZT%: P2%=-1: GOSUB PR_MEMORY
    GOSUB PR_MEMORY_SUMMARY
    END

  PRINT_ERROR:
    PRINT "Error: "+ER$
    ER%=-2:ER$=""
    RETURN

