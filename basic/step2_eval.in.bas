GOTO MAIN

REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

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

  GOSUB DEREF_A

  T=Z%(A,0)AND 31
  IF T=5 THEN GOTO EVAL_AST_SYMBOL
  IF T>=6 AND T<=8 THEN GOTO EVAL_AST_SEQ

  REM scalar: deref to actual value and inc ref cnt
  R=A:GOSUB DEREF_R
  Z%(R,0)=Z%(R,0)+32
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    H=E:K=A:GOSUB HASHMAP_GET
    GOSUB DEREF_R
    IF R3=0 THEN ER=-1:E$="'"+S$(Z%(A,1))+"' not found":GOTO EVAL_AST_RETURN
    Z%(R,0)=Z%(R,0)+32
    GOTO EVAL_AST_RETURN

  EVAL_AST_SEQ:
    REM allocate the first entry (T already set above)
    L=0:N=0:GOSUB ALLOC

    REM push type of sequence
    Q=T:GOSUB PUSH_Q
    REM push sequence index
    Q=0:GOSUB PUSH_Q
    REM push future return value (new sequence)
    GOSUB PUSH_R
    REM push previous new sequence entry
    GOSUB PUSH_R

    EVAL_AST_SEQ_LOOP:
      REM check if we are done evaluating the source sequence
      IF Z%(A,1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM if hashmap, skip eval of even entries (keys)
      Q=3:GOSUB PEEK_Q_Q:T=Q
      REM get and update index
      GOSUB PEEK_Q_2
      Q=Q+1:GOSUB PUT_Q_2
      IF T=8 AND ((Q-1)AND 1)=0 THEN GOTO EVAL_AST_DO_REF
      GOTO EVAL_AST_DO_EVAL

      EVAL_AST_DO_REF:
        R=A+1:GOSUB DEREF_R: REM deref to target of referred entry
        Z%(R,0)=Z%(R,0)+32: REM inc ref cnt of referred value
        GOTO EVAL_AST_ADD_VALUE

      EVAL_AST_DO_EVAL:
        REM call EVAL for each entry
        A=A+1:CALL EVAL
        A=A-1
        GOSUB DEREF_R: REM deref to target of evaluated entry

      EVAL_AST_ADD_VALUE:

      REM update previous value pointer to evaluated entry
      GOSUB PEEK_Q
      Z%(Q+1,1)=R

      IF ER<>-2 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM allocate the next entry
      REM same new sequence entry type
      Q=3:GOSUB PEEK_Q_Q:T=Q
      L=0:N=0:GOSUB ALLOC

      REM update previous sequence entry value to point to new entry
      GOSUB PEEK_Q
      Z%(Q,1)=R
      REM update previous ptr to current entry
      Q=R:GOSUB PUT_Q

      REM process the next sequence entry from source list
      A=Z%(A,1)

      GOTO EVAL_AST_SEQ_LOOP
    EVAL_AST_SEQ_LOOP_DONE:
      REM get return value (new seq), index, and seq type
      GOSUB PEEK_Q_1
      R=Q
      REM pop previous, return, index and type
      GOSUB POP_Q:GOSUB POP_Q:GOSUB POP_Q:GOSUB POP_Q
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

  GOSUB DEREF_A

  GOSUB LIST_Q
  IF R THEN GOTO APPLY_LIST
  REM ELSE
    CALL EVAL_AST
    GOTO EVAL_RETURN

  APPLY_LIST:
    GOSUB EMPTY_Q
    IF R THEN R=A:Z%(R,0)=Z%(R,0)+32:GOTO EVAL_RETURN

    EVAL_INVOKE:
      CALL EVAL_AST
      W=R

      REM if error, return f/args for release by caller
      IF ER<>-2 THEN GOTO EVAL_RETURN
      F=R+1

      AR=Z%(R,1): REM rest
      R=F:GOSUB DEREF_R:F=R
      IF (Z%(F,0)AND 31)<>9 THEN ER=-1:E$="apply of non-function":GOTO EVAL_RETURN
      GOSUB DO_FUNCTION
      AY=W:GOSUB RELEASE
      GOTO EVAL_RETURN

  EVAL_RETURN:

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
  R=AR+1:GOSUB DEREF_R:A=Z%(R,1)
  R=Z%(AR,1)+1:GOSUB DEREF_R:B=Z%(R,1)

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
  R1=0:R2=0
  GOSUB MAL_READ
  R1=R
  IF ER<>-2 THEN GOTO REP_DONE

  A=R:E=D:CALL EVAL
  R2=R
  IF ER<>-2 THEN GOTO REP_DONE

  A=R:GOSUB MAL_PRINT
  RT$=R$

  REP_DONE:
    REM Release memory from MAL_READ and EVAL
    IF R2<>0 THEN AY=R2:GOSUB RELEASE
    IF R1<>0 THEN AY=R1:GOSUB RELEASE
    R$=RT$
END SUB

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  LV=0

  REM create repl_env
  GOSUB HASHMAP:D=R

  REM + function
  A=1:GOSUB NATIVE_FUNCTION
  H=D:B$="+":C=R:GOSUB ASSOC1_S:D=R

  REM - function
  A=2:GOSUB NATIVE_FUNCTION
  H=D:B$="-":C=R:GOSUB ASSOC1_S:D=R

  REM * function
  A=3:GOSUB NATIVE_FUNCTION
  H=D:B$="*":C=R:GOSUB ASSOC1_S:D=R

  REM / function
  A=4:GOSUB NATIVE_FUNCTION
  H=D:B$="/":C=R:GOSUB ASSOC1_S:D=R

  ZT=ZI: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT

    A$=R$:CALL REP: REM call REP

    IF ER<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM GOSUB PR_MEMORY_SUMMARY
    END

  PRINT_ERROR:
    PRINT "Error: "+E$
    ER=-2:E$=""
    RETURN

