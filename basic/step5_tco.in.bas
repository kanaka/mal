GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'
REM $INCLUDE: 'env.in.bas'
REM $INCLUDE: 'core.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ(A$) -> R%
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL_AST(A%, E%) -> R%
REM called using GOTO to avoid basic return address stack usage
REM top of stack should have return label index
EVAL_AST:
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

    REM pop EVAL AST return label/address
    RN%=ZZ%(ZL%):ZL%=ZL%-1
    ON RN% GOTO EVAL_AST_RETURN_1,EVAL_AST_RETURN_2,EVAL_AST_RETURN_3
    RETURN

REM EVAL(A%, E%)) -> R%
EVAL:
  LV%=LV%+1: REM track basic return stack level

  REM push A% and E% on the stack
  ZL%=ZL%+2:ZZ%(ZL%-1)=E%:ZZ%(ZL%)=A%

  EVAL_TCO_RECUR:

  REM AZ%=A%: GOSUB PR_STR
  REM PRINT "EVAL: "+R$+"("+STR$(A%)+"), LV%:"+STR$(LV%)

  GOSUB DEREF_A

  GOSUB LIST_Q
  IF R% THEN GOTO APPLY_LIST
  REM ELSE
    REM push EVAL_AST return label/address
    ZL%=ZL%+1:ZZ%(ZL%)=1
    GOTO EVAL_AST
    EVAL_AST_RETURN_1:

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
    IF A$="do" THEN GOTO EVAL_DO
    IF A$="if" THEN GOTO EVAL_IF
    IF A$="fn*" THEN GOTO EVAL_FN
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

      REM set a1 in env to a2
      K%=A1%:V%=R%:GOSUB ENV_SET
      GOTO EVAL_RETURN

    EVAL_LET:
      REM PRINT "let*"
      GOSUB EVAL_GET_A2: REM set a1% and a2%

      E4%=E%: REM save the current environment for release

      REM create new environment with outer as current environment
      EO%=E%:GOSUB ENV_NEW
      E%=R%
      EVAL_LET_LOOP:
        IF Z%(A1%,1)=0 THEN GOTO EVAL_LET_LOOP_DONE

        REM push A1%
        ZL%=ZL%+1:ZZ%(ZL%)=A1%
        REM eval current A1 odd element
        A%=Z%(A1%,1)+1:GOSUB EVAL
        REM pop A1%
        A1%=ZZ%(ZL%):ZL%=ZL%-1

        REM set environment: even A1% key to odd A1% eval'd above
        K%=A1%+1:V%=R%:GOSUB ENV_SET
        AY%=R%:GOSUB RELEASE: REM release our use, ENV_SET took ownership

        REM skip to the next pair of A1% elements
        A1%=Z%(Z%(A1%,1),1)
        GOTO EVAL_LET_LOOP
      EVAL_LET_LOOP_DONE:
        REM release previous env (if not root repl_env) because our
        REM new env refers to it and we no longer need to track it
        REM (since we are TCO recurring)
        IF E4%<>RE% THEN AY%=E4%:GOSUB RELEASE

        A%=A2%:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_DO:
      A%=Z%(A%,1): REM rest

      REM TODO: TCO

      REM push EVAL_AST return label/address
      ZL%=ZL%+1:ZZ%(ZL%)=2
      GOTO EVAL_AST
      EVAL_AST_RETURN_2:

      ZL%=ZL%+1:ZZ%(ZL%)=R%: REM push eval'd list
      A%=R%:GOSUB LAST: REM return the last element
      AY%=ZZ%(ZL%):ZL%=ZL%-1: REM pop eval'd list
      GOSUB RELEASE: REM release the eval'd list
      GOTO EVAL_RETURN

    EVAL_IF:
      GOSUB EVAL_GET_A1: REM set a1%
      REM push A%
      ZL%=ZL%+1:ZZ%(ZL%)=A%
      A%=A1%:GOSUB EVAL
      REM pop A%
      A%=ZZ%(ZL%):ZL%=ZL%-1
      IF (R%=0) OR (R%=1) THEN GOTO EVAL_IF_FALSE

      EVAL_IF_TRUE:
        AY%=R%:GOSUB RELEASE
        GOSUB EVAL_GET_A2: REM set a1% and a2% after EVAL
        A%=A2%:GOTO EVAL_TCO_RECUR: REM TCO loop
      EVAL_IF_FALSE:
        AY%=R%:GOSUB RELEASE
        REM if no false case (A3%), return nil
        IF Z%(Z%(Z%(A%,1),1),1)=0 THEN R%=0:GOTO EVAL_RETURN
        GOSUB EVAL_GET_A3: REM set a1% - a3% after EVAL
        A%=A3%:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_FN:
      GOSUB EVAL_GET_A2: REM set a1% and a2%
      A%=A2%:P%=A1%:GOSUB MAL_FUNCTION
      GOTO EVAL_RETURN

    EVAL_INVOKE:
      REM push EVAL_AST return label/address
      ZL%=ZL%+1:ZZ%(ZL%)=3
      GOTO EVAL_AST
      EVAL_AST_RETURN_3:

      REM if error, return f/args for release by caller
      IF ER%<>0 THEN GOTO EVAL_RETURN

      REM push f/args for release after call
      ZL%=ZL%+1:ZZ%(ZL%)=R%

      F%=R%+1

      AR%=Z%(R%,1): REM rest
      R%=F%:GOSUB DEREF_R:F%=R%

      IF (Z%(F%,0)AND15)=9 THEN GOTO EVAL_DO_FUNCTION
      IF (Z%(F%,0)AND15)=10 THEN GOTO EVAL_DO_MAL_FUNCTION

      REM if error, pop and return f/args for release by caller
      R%=ZZ%(ZL%):ZL%=ZL%-1
      ER%=1:ER$="apply of non-function":GOTO EVAL_RETURN

      EVAL_DO_FUNCTION:
        GOSUB DO_FUNCTION

        REM pop and release f/args
        AY%=ZZ%(ZL%):ZL%=ZL%-1:GOSUB RELEASE
        GOTO EVAL_RETURN

      EVAL_DO_MAL_FUNCTION:
        E4%=E%: REM save the current environment for release

        REM create new environ using env stored with function
        EO%=Z%(F%+1,1):BI%=Z%(F%+1,0):EX%=AR%:GOSUB ENV_NEW_BINDS

        REM release previous env if it is not the top one on the
        REM stack (ZZ%(ZL%-2)) because our new env refers to it and
        REM we no longer need to track it (since we are TCO recurring)
        IF E4%<>ZZ%(ZL%-2) THEN AY%=E4%:GOSUB RELEASE

        REM claim the AST before releasing the list containing it
        A%=Z%(F%,1):Z%(A%,0)=Z%(A%,0)+16
        REM add AST to pending release queue to free as soon as EVAL
        REM actually returns (LV%+1)
        ZM%=ZM%+1:ZR%(ZM%,0)=A%:ZR%(ZM%,1)=LV%+1

        REM pop and release f/args
        AY%=ZZ%(ZL%):ZL%=ZL%-1:GOSUB RELEASE

        REM A% set above
        E%=R%:GOTO EVAL_TCO_RECUR: REM TCO loop

  EVAL_RETURN:
    REM AZ%=R%: PR%=1: GOSUB PR_STR
    REM PRINT "EVAL_RETURN R%: ["+R$+"] ("+STR$(R%)+"), LV%:"+STR$(LV%)+",ER%:"+STR$(ER%)

    REM release environment if not the top one on the stack
    IF E%<>ZZ%(ZL%-1) THEN AY%=E%:GOSUB RELEASE

    LV%=LV%-1: REM track basic return stack level

    REM release everything we couldn't release earlier
    GOSUB RELEASE_PEND

    REM trigger GC
    TA%=FRE(0)

    REM pop A% and E% off the stack
    E%=ZZ%(ZL%-1):A%=ZZ%(ZL%):ZL%=ZL%-2

    RETURN

REM PRINT(A%) -> R$
MAL_PRINT:
  AZ%=A%:PR%=1:GOSUB PR_STR
  RETURN

REM RE(A$) -> R%
REM Assume RE% has repl_env
REM caller must release result
RE:
  R1%=0
  GOSUB MAL_READ
  R1%=R%
  IF ER%<>0 THEN GOTO REP_DONE

  A%=R%:E%=RE%:GOSUB EVAL

  REP_DONE:
    REM Release memory from MAL_READ
    IF R1%<>0 THEN AY%=R1%:GOSUB RELEASE
    RETURN: REM caller must release result of EVAL

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

  REM core.EXT: defined in Basic
  E%=RE%:GOSUB INIT_CORE_NS: REM set core functions in repl_env

  ZT%=ZI%: REM top of memory after base repl_env

  REM core.mal: defined using the language itself
  A$="(def! not (fn* (a) (if a false true)))"
  GOSUB RE:AY%=R%:GOSUB RELEASE

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

