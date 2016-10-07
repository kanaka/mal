GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'
REM $INCLUDE: 'env.in.bas'

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

  IF ER%<>0 THEN GOTO EVAL_AST_RETURN

  GOSUB DEREF_A

  T%=Z%(A%,0)AND15
  IF T%=5 THEN GOTO EVAL_AST_SYMBOL
  IF T%>=6 AND T%<=8 THEN GOTO EVAL_AST_SEQ

  REM scalar: deref to actual value and inc ref cnt
  R%=A%:GOSUB DEREF_R
  Z%(R%,0)=Z%(R%,0)+16
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    K%=A%:GOSUB ENV_GET
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

      IF ER%<>0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

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
      REM if no error, get return value (new seq)
      IF ER%=0 THEN R%=ZZ%(ZL%-1)
      REM otherwise, free the return value and return nil
      IF ER%<>0 THEN R%=0:AY%=ZZ%(ZL%-1):GOSUB RELEASE

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

    A0%=A%+1
    R%=A0%:GOSUB DEREF_R:A0%=R%

    REM get symbol in A$
    IF (Z%(A0%,0)AND15)<>5 THEN A$=""
    IF (Z%(A0%,0)AND15)=5 THEN A$=ZS$(Z%(A0%,1))

    IF A$="def!" THEN GOTO EVAL_DEF
    IF A$="let*" THEN GOTO EVAL_LET
    GOTO EVAL_INVOKE

    EVAL_GET_A3:
      A3%=Z%(Z%(Z%(A%,1),1),1)+1
      R%=A3%:GOSUB DEREF_R:A3%=R%
    EVAL_GET_A2:
      A2%=Z%(Z%(A%,1),1)+1
      R%=A2%:GOSUB DEREF_R:A2%=R%
    EVAL_GET_A1:
      A1%=Z%(A%,1)+1
      R%=A1%:GOSUB DEREF_R:A1%=R%
      RETURN

    EVAL_DEF:
      REM PRINT "def!"
      GOSUB EVAL_GET_A2: REM set a1% and a2%

      ZL%=ZL%+1:ZZ%(ZL%)=A1%: REM push A1%
      A%=A2%:GOSUB EVAL: REM eval a2
      A1%=ZZ%(ZL%):ZL%=ZL%-1: REM pop A1%

      IF ER%<>0 THEN GOTO EVAL_RETURN

      REM set a1 in env to a2
      K%=A1%:V%=R%:GOSUB ENV_SET
      GOTO EVAL_RETURN

    EVAL_LET:
      REM PRINT "let*"
      GOSUB EVAL_GET_A2: REM set a1% and a2%

      ZL%=ZL%+1:ZZ%(ZL%)=A2%: REM push/save A2%
      REM create new environment with outer as current environment
      EO%=E%:GOSUB ENV_NEW
      E%=R%
      EVAL_LET_LOOP:
        IF Z%(A1%,1)=0 THEN GOTO EVAL_LET_LOOP_DONE

        ZL%=ZL%+1:ZZ%(ZL%)=A1%: REM push A1%
        REM eval current A1 odd element
        A%=Z%(A1%,1)+1:GOSUB EVAL
        A1%=ZZ%(ZL%):ZL%=ZL%-1: REM pop A1%

        REM set environment: even A1% key to odd A1% eval'd above
        K%=A1%+1:V%=R%:GOSUB ENV_SET
        AY%=R%:GOSUB RELEASE: REM release our use, ENV_SET took ownership

        REM skip to the next pair of A1% elements
        A1%=Z%(Z%(A1%,1),1)
        GOTO EVAL_LET_LOOP

      EVAL_LET_LOOP_DONE:
        A2%=ZZ%(ZL%):ZL%=ZL%-1: REM pop A2%
        A%=A2%:GOSUB EVAL: REM eval a2 using let_env
        GOTO EVAL_RETURN
    EVAL_INVOKE:
      GOSUB EVAL_AST
      R3%=R%

      REM if error, return f/args for release by caller
      IF ER%<>0 THEN GOTO EVAL_RETURN
      F%=R%+1

      AR%=Z%(R%,1): REM rest
      R%=F%:GOSUB DEREF_R:F%=R%
      IF (Z%(F%,0)AND15)<>9 THEN ER%=1:ER$="apply of non-function":GOTO EVAL_RETURN
      GOSUB DO_FUNCTION
      AY%=R3%:GOSUB RELEASE
      GOTO EVAL_RETURN

  EVAL_RETURN:
    REM AZ%=R%: PR%=1: GOSUB PR_STR
    REM PRINT "EVAL_RETURN R%: ["+R$+"] ("+STR$(R%)+"), LV%:"+STR$(LV%)+",ER%:"+STR$(ER%)

    REM release environment if not the top one on the stack
    IF E%<>ZZ%(ZL%-1) THEN AY%=E%:GOSUB RELEASE

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
  ER%=1:ER$="unknown function"+STR$(FF%):RETURN

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
  IF ER%<>0 THEN GOTO REP_DONE

  A%=R%:E%=RE%:GOSUB EVAL
  R2%=R%
  IF ER%<>0 THEN GOTO REP_DONE

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
  EO%=-1:GOSUB ENV_NEW:RE%=R%

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

  ZT%=ZI%: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EOF=1 THEN GOTO QUIT

    A$=R$:GOSUB REP: REM call REP

    IF ER%<>0 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM P1%=ZT%: P2%=-1: GOSUB PR_MEMORY
    GOSUB PR_MEMORY_SUMMARY
    END

  PRINT_ERROR:
    PRINT "Error: "+ER$
    ER%=0:ER$=""
    RETURN

