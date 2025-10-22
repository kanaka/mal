GOTO MAIN

REM $INCLUDE: 'mem.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ is inlined in RE

REM EVAL_AST(A, E) -> R
SUB EVAL_AST
  REM push A on the stack
  GOSUB PUSH_A

  IF ER<>-2 THEN GOTO EVAL_AST_RETURN

  GOSUB TYPE_A
  IF T<6 OR 8<T THEN R=-1:ER=-1:E$="EVAL_AST: bad type":GOTO EVAL_AST_RETURN

    REM setup the stack for the loop
    GOSUB MAP_LOOP_START

    EVAL_AST_SEQ_LOOP:
      REM check if we are done evaluating the source sequence
      IF Z%(A+1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM call EVAL for each entry
      GOSUB PUSH_A
      IF T<>8 THEN A=Z%(A+2)
      IF T=8 THEN A=Z%(A+3)
      Q=T:GOSUB PUSH_Q: REM push/save type
      CALL EVAL
      GOSUB POP_Q:T=Q: REM pop/restore type
      GOSUB POP_A
      M=R

      REM if error, release the unattached element
      REM TODO: is R=0 correct?
      IF ER<>-2 THEN AY=R:GOSUB RELEASE:R=0:GOTO EVAL_AST_SEQ_LOOP_DONE

      REM for hash-maps, copy the key (inc ref since we are going to
      REM release it below)
      IF T=8 THEN N=M:M=Z%(A+2):Z%(M)=Z%(M)+32

      REM update the return sequence structure
      REM release N (and M if T=8) since seq takes full ownership
      C=1:GOSUB MAP_LOOP_UPDATE

      REM process the next sequence entry from source list
      A=Z%(A+1)

      GOTO EVAL_AST_SEQ_LOOP
    EVAL_AST_SEQ_LOOP_DONE:
      REM cleanup stack and get return value
      GOSUB MAP_LOOP_DONE
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    REM pop A off the stack
    GOSUB POP_A
END SUB

REM EVAL(A, E) -> R
SUB EVAL
  LV=LV+1: REM track basic return stack level

  REM push A on the stack
  GOSUB PUSH_A

  REM PRINT "EVAL A:"+STR$(A)+",X:"+STR$(X)+",LV:"+STR$(LV)+",FRE:"+STR$(FRE(0))

  IF ER<>-2 THEN GOTO EVAL_RETURN

  REM AZ=A:B=1:GOSUB PR_STR
  REM PRINT "EVAL: "+R$+" [A:"+STR$(A)+", LV:"+STR$(LV)+"]"

  GOSUB TYPE_A
  T=T-4
  IF 0<T THEN ON T GOTO EVAL_SYMBOL,APPLY_LIST,EVAL_VECTOR,EVAL_MAP

  REM ELSE
    R=A
    GOSUB INC_REF_R
    GOTO EVAL_RETURN

  EVAL_SYMBOL:
    H=E:B$=S$(Z%(A+1)):GOSUB HASHMAP_GET
    IF R3=0 THEN R=-1:ER=-1:E$="'"+B$+"' not found":GOTO EVAL_RETURN
    GOSUB INC_REF_R
    GOTO EVAL_RETURN

  EVAL_MAP:
  EVAL_VECTOR:
    CALL EVAL_AST
    GOTO EVAL_RETURN

  APPLY_LIST:

    GOSUB EMPTY_Q
    IF R THEN R=A:GOSUB INC_REF_R:GOTO EVAL_RETURN

    A0=Z%(A+2)

    EVAL_INVOKE:

      REM evaluate A0
      GOSUB PUSH_A
      A=A0:CALL EVAL
      GOSUB POP_A
      IF ER<>-2 THEN GOTO EVAL_RETURN

      REM set F, push it in the stack for release after call
      GOSUB PUSH_R
      F=R

      GOSUB TYPE_F
      T=T-8
      IF 0<T THEN ON T GOTO EVAL_DO_FUNCTION

      REM if error, pop and return f for release by caller
      GOSUB POP_R
      ER=-1:E$="apply of non-function":GOTO EVAL_RETURN

      EVAL_DO_FUNCTION:
        REM regular function

        REM Evaluate the arguments
        A=Z%(A+1):CALL EVAL_AST
        IF ER<>-2 THEN GOSUB POP_Q:AY=Q:GOSUB RELEASE:GOTO EVAL_RETURN

        REM set F and AR, push AR (after F) in the stack for release after call
        GOSUB PEEK_Q:F=Q
        GOSUB PUSH_R
        AR=R

        GOSUB DO_FUNCTION

        REM pop and release f/args
        GOSUB POP_Q:AY=Q:GOSUB RELEASE
        GOSUB POP_Q:AY=Q
        GOSUB RELEASE

  EVAL_RETURN:
    REM AZ=R: B=1: GOSUB PR_STR
    REM PRINT "EVAL_RETURN R: ["+R$+"] ("+STR$(R)+"), LV:"+STR$(LV)+",ER:"+STR$(ER)

    LV=LV-1: REM track basic return stack level

    REM release everything we couldn't release earlier
    GOSUB RELEASE_PEND

    REM trigger GC
    #cbm T=FRE(0)
    #qbasic T=0

    REM pop A off the stack
    GOSUB POP_A

END SUB

REM DO_FUNCTION(F, AR)
DO_FUNCTION:
  REM Get the function number
  G=Z%(F+1)

  REM Get argument values
  A=Z%(Z%(AR+2)+1)
  B=Z%(Z%(Z%(AR+1)+2)+1)

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

REM PRINT is inlined in REP

REM RE(A$) -> R
REM Assume D has repl_env
REM caller must release result
RE:
  R1=-1
  GOSUB READ_STR: REM inlined READ
  R1=R
  IF ER<>-2 THEN GOTO RE_DONE

  A=R:E=D:CALL EVAL

  RE_DONE:
    REM Release memory from READ
    AY=R1:GOSUB RELEASE
    RETURN: REM caller must release result of EVAL

REM REP(A$) -> R$
REM Assume D has repl_env
SUB REP
  R2=-1

  GOSUB RE
  R2=R
  IF ER<>-2 THEN GOTO REP_DONE

  AZ=R:B=1:GOSUB PR_STR: REM inlined PRINT

  REP_DONE:
    REM Release memory from EVAL
    AY=R2:GOSUB RELEASE
END SUB

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  LV=0

  REM create repl_env
  GOSUB HASHMAP:D=R

  REM + function
  T=9:L=1:GOSUB ALLOC: REM native function
  H=D:B$="+":C=R:GOSUB ASSOC1_S:D=R

  REM - function
  T=9:L=2:GOSUB ALLOC: REM native function
  H=D:B$="-":C=R:GOSUB ASSOC1_S:D=R

  REM * function
  T=9:L=3:GOSUB ALLOC: REM native function
  H=D:B$="*":C=R:GOSUB ASSOC1_S:D=R

  REM / function
  T=9:L=4:GOSUB ALLOC: REM native function
  H=D:B$="/":C=R:GOSUB ASSOC1_S:D=R

  ZT=ZI: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT
    IF R$="" THEN GOTO REPL_LOOP

    A$=R$:CALL REP

    IF ER<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM GOSUB PR_MEMORY_SUMMARY_SMALL
    REM GOSUB PR_MEMORY_MAP
    REM P1=0:P2=ZI:GOSUB PR_MEMORY
    REM P1=D:GOSUB PR_OBJECT
    REM P1=ZK:GOSUB PR_OBJECT
    #cbm END
    #qbasic SYSTEM

  PRINT_ERROR:
    PRINT "Error: "+E$
    ER=-2:E$=""
    RETURN
