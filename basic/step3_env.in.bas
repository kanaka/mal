GOTO MAIN

REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'
REM $INCLUDE: 'env.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ(A$) -> R
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL_AST(A, E) -> R
SUB EVAL_AST
  LV=LV+1

  REM push A and E on the stack
  Q=E:GOSUB PUSH_Q
  GOSUB PUSH_A

  IF ER<>-2 THEN GOTO EVAL_AST_RETURN

  T=Z%(A,0)AND 31
  IF T=5 THEN GOTO EVAL_AST_SYMBOL
  IF T>=6 AND T<=8 THEN GOTO EVAL_AST_SEQ

  REM scalar: deref to actual value and inc ref cnt
  R=A
  Z%(R,0)=Z%(R,0)+32
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    K=A:GOTO ENV_GET
    ENV_GET_RETURN:
    GOTO EVAL_AST_RETURN

  EVAL_AST_SEQ:
    REM setup the stack for the loop
    GOSUB MAP_LOOP_START

    EVAL_AST_SEQ_LOOP:
      REM check if we are done evaluating the source sequence
      IF Z%(A,1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM call EVAL for each entry
      GOSUB PUSH_A
      IF T<>8 THEN GOSUB VAL_A
      IF T=8 THEN A=Z%(A+1,1)
      Q=T:GOSUB PUSH_Q: REM push/save type
      CALL EVAL
      GOSUB POP_Q:T=Q: REM pop/restore type
      GOSUB POP_A

      REM if error, release the unattached element
      REM TODO: is R=0 correct?
      IF ER<>-2 THEN AY=R:GOSUB RELEASE:R=0:GOTO EVAL_AST_SEQ_LOOP_DONE

      REM for hash-maps, copy the key (inc ref since we are going to
      REM release it below)
      IF T=8 THEN M=Z%(A+1,0):Z%(M,0)=Z%(M,0)+32

      REM value evaluated above
      N=R

      REM update the return sequence structure
      REM release N (and M if T=8) since seq takes full ownership
      C=1:GOSUB MAP_LOOP_UPDATE

      REM process the next sequence entry from source list
      A=Z%(A,1)

      GOTO EVAL_AST_SEQ_LOOP
    EVAL_AST_SEQ_LOOP_DONE:
      REM cleanup stack and get return value
      GOSUB MAP_LOOP_DONE
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    REM pop A and E off the stack
    GOSUB POP_A
    GOSUB POP_Q:E=Q

    LV=LV-1
END SUB

REM EVAL(A, E) -> R
SUB EVAL
  LV=LV+1: REM track basic return stack level

  REM push A and E on the stack
  Q=E:GOSUB PUSH_Q
  GOSUB PUSH_A

  IF ER<>-2 THEN GOTO EVAL_RETURN

  REM AZ=A:B=1:GOSUB PR_STR
  REM PRINT "EVAL: "+R$+" [A:"+STR$(A)+", LV:"+STR$(LV)+"]"

  GOSUB LIST_Q
  IF R THEN GOTO APPLY_LIST
  REM ELSE
    CALL EVAL_AST
    GOTO EVAL_RETURN

  APPLY_LIST:
    GOSUB EMPTY_Q
    IF R THEN R=A:Z%(R,0)=Z%(R,0)+32:GOTO EVAL_RETURN

    A0=Z%(A+1,1)

    REM get symbol in A$
    IF (Z%(A0,0)AND 31)<>5 THEN A$=""
    IF (Z%(A0,0)AND 31)=5 THEN A$=S$(Z%(A0,1))

    IF A$="def!" THEN GOTO EVAL_DEF
    IF A$="let*" THEN GOTO EVAL_LET
    GOTO EVAL_INVOKE

    EVAL_GET_A3:
      R=Z%(Z%(Z%(A,1),1),1)
      GOSUB VAL_R:A3=R
    EVAL_GET_A2:
      R=Z%(Z%(A,1),1)
      GOSUB VAL_R:A2=R
    EVAL_GET_A1:
      R=Z%(A,1)
      GOSUB VAL_R:A1=R
      RETURN

    EVAL_DEF:
      REM PRINT "def!"
      GOSUB EVAL_GET_A2: REM set A1 and A2

      Q=A1:GOSUB PUSH_Q
      A=A2:CALL EVAL: REM eval a2
      GOSUB POP_Q:A1=Q

      IF ER<>-2 THEN GOTO EVAL_RETURN

      REM set a1 in env to a2
      K=A1:C=R:GOSUB ENV_SET
      GOTO EVAL_RETURN

    EVAL_LET:
      REM PRINT "let*"
      GOSUB EVAL_GET_A2: REM set A1 and A2

      Q=A2:GOSUB PUSH_Q: REM push/save A2
      REM create new environment with outer as current environment
      C=E:GOSUB ENV_NEW
      E=R
      EVAL_LET_LOOP:
        IF Z%(A1,1)=0 THEN GOTO EVAL_LET_LOOP_DONE

        Q=A1:GOSUB PUSH_Q: REM push A1
        REM eval current A1 odd element
        A=Z%(A1,1):GOSUB VAL_A:CALL EVAL
        GOSUB POP_Q:A1=Q: REM pop A1

        IF ER<>-2 THEN GOTO EVAL_LET_LOOP_DONE

        REM set environment: even A1 key to odd A1 eval'd above
        K=Z%(A1+1,1):C=R:GOSUB ENV_SET
        AY=R:GOSUB RELEASE: REM release our use, ENV_SET took ownership

        REM skip to the next pair of A1 elements
        A1=Z%(Z%(A1,1),1)
        GOTO EVAL_LET_LOOP

      EVAL_LET_LOOP_DONE:
        GOSUB POP_Q:A2=Q: REM pop A2
        A=A2:CALL EVAL: REM eval A2 using let_env
        GOTO EVAL_RETURN
    EVAL_INVOKE:
      CALL EVAL_AST
      W=R

      REM if error, return f/args for release by caller
      IF ER<>-2 THEN GOTO EVAL_RETURN

      AR=Z%(R,1): REM rest
      GOSUB VAL_R:F=R

      IF (Z%(F,0)AND 31)<>9 THEN R=-1:ER=-1:E$="apply of non-function":GOTO EVAL_INVOKE_DONE
      GOSUB DO_FUNCTION
      EVAL_INVOKE_DONE:
      AY=W:GOSUB RELEASE
      GOTO EVAL_RETURN

  EVAL_RETURN:
    REM release environment if not the top one on the stack
    GOSUB PEEK_Q_1
    IF E<>Q THEN AY=E:GOSUB RELEASE

    LV=LV-1: REM track basic return stack level

    REM trigger GC
    #cbm T=FRE(0)
    #qbasic T=0

    REM pop A and E off the stack
    GOSUB POP_A
    GOSUB POP_Q:E=Q

END SUB

REM DO_FUNCTION(F, AR)
DO_FUNCTION:
  AZ=F:GOSUB PR_STR
  F$=R$
  AZ=AR:GOSUB PR_STR
  AR$=R$

  REM Get the function number
  G=Z%(F,1)

  REM Get argument values
  R=AR:GOSUB VAL_R:A=Z%(R,1)
  R=Z%(AR,1):GOSUB VAL_R:B=Z%(R,1)

  REM Switch on the function number
  IF G=1 THEN GOTO DO_ADD
  IF G=2 THEN GOTO DO_SUB
  IF G=3 THEN GOTO DO_MULT
  IF G=4 THEN GOTO DO_DIV
  ER=-1:E$="unknown function"+STR$(G):RETURN

  DO_ADD:
    T=2:L=A+B:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE
  DO_SUB:
    T=2:L=A-B:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE
  DO_MULT:
    T=2:L=A*B:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE
  DO_DIV:
    T=2:L=A/B:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE

  DO_FUNCTION_DONE:
    RETURN

REM PRINT(A) -> R$
MAL_PRINT:
  AZ=A:B=1:GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
REM Assume D has repl_env
SUB REP
  R1=-1:R2=-1
  GOSUB MAL_READ
  R1=R
  IF ER<>-2 THEN GOTO REP_DONE

  A=R:E=D:CALL EVAL
  R2=R
  IF ER<>-2 THEN GOTO REP_DONE

  A=R:GOSUB MAL_PRINT

  REP_DONE:
    REM Release memory from MAL_READ and EVAL
    AY=R2:GOSUB RELEASE
    AY=R1:GOSUB RELEASE
END SUB

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  LV=0

  REM create repl_env
  C=0:GOSUB ENV_NEW:D=R

  E=D
  REM + function
  A=1:GOSUB NATIVE_FUNCTION
  B$="+":C=R:GOSUB ENV_SET_S

  REM - function
  A=2:GOSUB NATIVE_FUNCTION
  B$="-":C=R:GOSUB ENV_SET_S

  REM * function
  A=3:GOSUB NATIVE_FUNCTION
  B$="*":C=R:GOSUB ENV_SET_S

  REM / function
  A=4:GOSUB NATIVE_FUNCTION
  B$="/":C=R:GOSUB ENV_SET_S

  ZT=ZI: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT
    IF R$="" THEN GOTO REPL_LOOP

    A$=R$:CALL REP: REM call REP

    IF ER<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM GOSUB PR_MEMORY_SUMMARY_SMALL
    END

  PRINT_ERROR:
    PRINT "Error: "+E$
    ER=-2:E$=""
    RETURN

