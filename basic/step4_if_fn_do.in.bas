GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'
REM $INCLUDE: 'env.in.bas'
REM $INCLUDE: 'core.in.bas'

REM READ(A$) -> R%
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL_AST(A%, E%) -> R%
REM called using GOTO to avoid basic return address stack usage
REM top of stack should have return label index
EVAL_AST:
  REM push A% and E% on the stack
  ZL%=ZL%+2: ZZ%(ZL%-1)=E%: ZZ%(ZL%)=A%

  IF ER%=1 THEN GOTO EVAL_AST_RETURN

  REM AZ%=A%: GOSUB PR_STR
  REM PRINT "EVAL_AST: " + R$ + "(" + STR$(R%) + ")"

  T%=Z%(A%,0)
  IF T%=5 THEN EVAL_AST_SYMBOL
  IF T%=6 THEN EVAL_AST_SEQ
  IF T%=8 THEN EVAL_AST_SEQ
  IF T%=10 THEN EVAL_AST_SEQ
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
      IF (ZZ%(ZL%-3)=10) AND ((ZZ%(ZL%-2) AND 1)=0) THEN GOTO EVAL_AST_SEQ_SKIP

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
    REM pop A% and E% off the stack
    E%=ZZ%(ZL%-1): A%=ZZ%(ZL%): ZL%=ZL%-2

    RN%=ZZ%(ZL%): ZL%=ZL%-1
    IF RN%=1 GOTO EVAL_AST_RETURN_1
    IF RN%=2 GOTO EVAL_AST_RETURN_2
    IF RN%=3 GOTO EVAL_AST_RETURN_3
    RETURN

REM EVAL(A%, E%)) -> R%
EVAL:
  LV%=LV%+1: REM track basic return stack level

  REM push A% and E% on the stack
  ZL%=ZL%+2: ZZ%(ZL%-1)=E%: ZZ%(ZL%)=A%

  EVAL_TCO_RECUR:

  IF ER%=1 THEN GOTO EVAL_RETURN

  REM AZ%=A%: GOSUB PR_STR
  REM PRINT "EVAL: " + R$ + "(" + STR$(R%) + "), DEPTH: " + STR$(LV%)
  REM PRINT "EVAL level: " + STR$(LV%)

  GOSUB LIST_Q
  IF R% THEN GOTO APPLY_LIST
  REM ELSE
    REM push EVAL_AST return label/address
    ZL%=ZL%+1: ZZ%(ZL%)=1
    GOTO EVAL_AST
    EVAL_AST_RETURN_1:

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
    IF A$="do" THEN GOTO EVAL_DO
    IF A$="if" THEN GOTO EVAL_IF
    IF A$="fn*" THEN GOTO EVAL_FN
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
      REM push A1%
      ZL%=ZL%+1: ZZ%(ZL%)=A1%
      A%=A2%: GOSUB EVAL: REM eval a2
      REM pop A1%
      A1%=ZZ%(ZL%): ZL%=ZL%-1
      REM set a1 in env to a2
      K%=A1%: V%=R%: GOSUB ENV_SET
      GOTO EVAL_RETURN
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
        GOTO EVAL_RETURN
    EVAL_DO:
      A%=Z%(A%,1): REM rest

      REM push EVAL_AST return label/address
      ZL%=ZL%+1: ZZ%(ZL%)=2
      GOTO EVAL_AST
      EVAL_AST_RETURN_2:

      A%=R%: GOSUB LAST: REM return the last element
      GOTO EVAL_RETURN
    EVAL_IF:
      GOSUB EVAL_GET_A1: REM set a1%
      REM push A%
      ZL%=ZL%+1: ZZ%(ZL%)=A%
      A%=A1%: GOSUB EVAL
      REM pop A%
      A%=ZZ%(ZL%): ZL%=ZL%-1
      IF (R%=0) OR (R%=1) THEN GOTO EVAL_IF_FALSE

      EVAL_IF_TRUE:
        GOSUB EVAL_GET_A2: REM set a1% and a2% after EVAL
        A%=A2%: GOTO EVAL_TCO_RECUR
      EVAL_IF_FALSE:
        REM if no false case (A3%), return nil
        IF Z%(Z%(Z%(A%,1),1),1)=0 THEN R%=0: GOTO EVAL_RETURN
        GOSUB EVAL_GET_A3: REM set a1% - a3% after EVAL
        A%=A3%: GOTO EVAL_TCO_RECUR
    EVAL_FN:
      GOSUB EVAL_GET_A2: REM set a1% and a2%
      A%=A2%: P%=A1%: GOSUB MAL_FUNCTION
      GOTO EVAL_RETURN
    EVAL_INVOKE:
      REM push EVAL_AST return label/address
      ZL%=ZL%+1: ZZ%(ZL%)=3
      GOTO EVAL_AST
      EVAL_AST_RETURN_3:

      IF ER%=1 THEN GOTO EVAL_RETURN
      F%=R%+1
      AR%=Z%(R%,1): REM rest
      R%=F%: GOSUB DEREF
      F%=R%

      IF Z%(F%,0)=12 THEN GOTO EVAL_DO_FUNCTION
      IF Z%(F%,0)=13 THEN GOTO EVAL_DO_MAL_FUNCTION
      ER%=1: ER$="apply of non-function": GOTO EVAL_RETURN
      EVAL_DO_FUNCTION:
        GOSUB DO_FUNCTION
        GOTO EVAL_RETURN
      EVAL_DO_MAL_FUNCTION:
        EO%=Z%(F%+1,1): BI%=Z%(F%+1,0): EX%=AR%: GOSUB ENV_NEW_BINDS
        A%=Z%(F%,1): E%=R%: GOTO EVAL_TCO_RECUR

  EVAL_RETURN:
    REM trigger GC
    T8%=FRE(0)
    REM pop A% and E% off the stack
    E%=ZZ%(ZL%-1): A%=ZZ%(ZL%): ZL%=ZL%-2

    LV%=LV%-1: REM track basic return stack level
    RETURN

REM PRINT(A%) -> R$
MAL_PRINT:
  AZ%=A%: PR%=1: GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
REM Assume RE% has repl_env
REP:
  LV%=LV%+1: REM track basic return stack level

  GOSUB MAL_READ
  IF ER% THEN GOTO REP_RETURN
  A%=R%: E%=RE%: GOSUB EVAL
  IF ER% THEN GOTO REP_RETURN
  A%=R%: GOSUB MAL_PRINT
  IF ER% THEN GOTO REP_RETURN
  REP_RETURN:
    LV%=LV%-1: REM track basic return stack level
    RETURN

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  LV%=0

  REM create repl_env
  EO%=-1: GOSUB ENV_NEW
  RE%=R%

  REM set core functions in repl_env
  E%=RE%: GOSUB INIT_CORE_NS

  REM AZ%=ZE%(RE%): GOSUB PR_STR
  REM PRINT "env: " + R$ + "(" + STR$(RE%) + ")"

  REM B% = PEEK(57) + PEEK(58) * 256
  REM PRINT "57/58%: " + STR$(B%)

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

