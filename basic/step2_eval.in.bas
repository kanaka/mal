GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
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
  X=X+2:X%(X-1)=E:X%(X)=A

  IF ER<>-2 THEN GOTO EVAL_AST_RETURN

  GOSUB DEREF_A

  T=Z%(A,0)AND31
  IF T=5 THEN GOTO EVAL_AST_SYMBOL
  IF T>=6 AND T<=8 THEN GOTO EVAL_AST_SEQ

  REM scalar: deref to actual value and inc ref cnt
  R=A:GOSUB DEREF_R
  Z%(R,0)=Z%(R,0)+32
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    H=E:K=A:GOSUB HASHMAP_GET
    GOSUB DEREF_R
    IF T3=0 THEN ER=-1:ER$="'"+S$(Z%(A,1))+"' not found":GOTO EVAL_AST_RETURN
    Z%(R,0)=Z%(R,0)+32
    GOTO EVAL_AST_RETURN

  EVAL_AST_SEQ:
    REM allocate the first entry (T already set above)
    L=0:N=0:GOSUB ALLOC

    REM make space on the stack
    X=X+4
    REM push type of sequence
    X%(X-3)=T
    REM push sequence index
    X%(X-2)=-1
    REM push future return value (new sequence)
    X%(X-1)=R
    REM push previous new sequence entry
    X%(X)=R

    EVAL_AST_SEQ_LOOP:
      REM update index
      X%(X-2)=X%(X-2)+1

      REM check if we are done evaluating the source sequence
      IF Z%(A,1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM if hashmap, skip eval of even entries (keys)
      IF (X%(X-3)=8) AND ((X%(X-2)AND1)=0) THEN GOTO EVAL_AST_DO_REF
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
      Z%(X%(X)+1,1)=R

      IF ER<>-2 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM allocate the next entry
      REM same new sequence entry type
      T=X%(X-3):L=0:N=0:GOSUB ALLOC

      REM update previous sequence entry value to point to new entry
      Z%(X%(X),1)=R
      REM update previous ptr to current entry
      X%(X)=R

      REM process the next sequence entry from source list
      A=Z%(A,1)

      GOTO EVAL_AST_SEQ_LOOP
    EVAL_AST_SEQ_LOOP_DONE:
      REM get return value (new seq), index, and seq type
      R=X%(X-1)
      REM pop previous, return, index and type
      X=X-4
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    REM pop A and E off the stack
    E=X%(X-1):A=X%(X):X=X-2

    LV=LV-1
END SUB

REM EVAL(A, E) -> R
SUB EVAL
  LV=LV+1: REM track basic return stack level

  REM push A and E on the stack
  X=X+2:X%(X-1)=E:X%(X)=A

  IF ER<>-2 THEN GOTO EVAL_RETURN

  REM AZ=A:PR=1:GOSUB PR_STR
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
      R3=R

      REM if error, return f/args for release by caller
      IF ER<>-2 THEN GOTO EVAL_RETURN
      F=R+1

      AR=Z%(R,1): REM rest
      R=F:GOSUB DEREF_R:F=R
      IF (Z%(F,0)AND31)<>9 THEN ER=-1:ER$="apply of non-function":GOTO EVAL_RETURN
      GOSUB DO_FUNCTION
      AY=R3:GOSUB RELEASE
      GOTO EVAL_RETURN

  EVAL_RETURN:

    LV=LV-1: REM track basic return stack level


    REM trigger GC
    TA=FRE(0)

    REM pop A and E off the stack
    E=X%(X-1):A=X%(X):X=X-2

END SUB

REM DO_FUNCTION(F, AR)
DO_FUNCTION:
  AZ=F:GOSUB PR_STR
  F$=R$
  AZ=AR:GOSUB PR_STR
  AR$=R$

  REM Get the function number
  FF=Z%(F,1)

  REM Get argument values
  R=AR+1:GOSUB DEREF_R:AA=Z%(R,1)
  R=Z%(AR,1)+1:GOSUB DEREF_R:AB=Z%(R,1)

  REM Switch on the function number
  IF FF=1 THEN GOTO DO_ADD
  IF FF=2 THEN GOTO DO_SUB
  IF FF=3 THEN GOTO DO_MULT
  IF FF=4 THEN GOTO DO_DIV
  ER=-1:ER$="unknown function"+STR$(FF):RETURN

  DO_ADD:
    T=2:L=AA+AB:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE
  DO_SUB:
    T=2:L=AA-AB:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE
  DO_MULT:
    T=2:L=AA*AB:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE
  DO_DIV:
    T=2:L=AA/AB:GOSUB ALLOC
    GOTO DO_FUNCTION_DONE

  DO_FUNCTION_DONE:
    RETURN

REM PRINT(A) -> R$
MAL_PRINT:
  AZ=A:PR=1:GOSUB PR_STR
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
  H=D:K$="+":V=R:GOSUB ASSOC1_S:D=R

  REM - function
  A=2:GOSUB NATIVE_FUNCTION
  H=D:K$="-":V=R:GOSUB ASSOC1_S:D=R

  REM * function
  A=3:GOSUB NATIVE_FUNCTION
  H=D:K$="*":V=R:GOSUB ASSOC1_S:D=R

  REM / function
  A=4:GOSUB NATIVE_FUNCTION
  H=D:K$="/":V=R:GOSUB ASSOC1_S:D=R

  ZT=ZI: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EOF=1 THEN GOTO QUIT

    A$=R$:CALL REP: REM call REP

    IF ER<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM GOSUB PR_MEMORY_SUMMARY
    END

  PRINT_ERROR:
    PRINT "Error: "+ER$
    ER=-2:ER$=""
    RETURN

