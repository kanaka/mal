GOTO MAIN

REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'
REM $INCLUDE: 'env.in.bas'
REM $INCLUDE: 'core.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ(A$) -> R
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL_AST(A, E) -> R
SUB EVAL_AST
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
    K=A:GOTO ENV_GET
    ENV_GET_RETURN:
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
      GOSUB PEEK_Q_1
      REM if no error, get return value (new seq)
      IF ER=-2 THEN R=Q
      REM otherwise, free the return value and return nil
      IF ER<>-2 THEN R=0:AY=Q:GOSUB RELEASE

      REM pop previous, return, index and type
      GOSUB POP_Q:GOSUB POP_Q:GOSUB POP_Q:GOSUB POP_Q
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    REM pop A and E off the stack
    GOSUB POP_A
    GOSUB POP_Q:E=Q
END SUB

REM EVAL(A, E) -> R
SUB EVAL
  LV=LV+1: REM track basic return stack level

  REM push A and E on the stack
  Q=E:GOSUB PUSH_Q
  GOSUB PUSH_A

  REM PRINT "EVAL A:"+STR$(A)+",X:"+STR$(X)+",LV:"+STR$(LV)+",FRE:"+STR$(FRE(0))

  EVAL_TCO_RECUR:

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

    A0=A+1
    R=A0:GOSUB DEREF_R:A0=R

    REM get symbol in A$
    IF (Z%(A0,0)AND 31)<>5 THEN A$=""
    IF (Z%(A0,0)AND 31)=5 THEN A$=S$(Z%(A0,1))

    IF A$="def!" THEN GOTO EVAL_DEF
    IF A$="let*" THEN GOTO EVAL_LET
    IF A$="do" THEN GOTO EVAL_DO
    IF A$="if" THEN GOTO EVAL_IF
    IF A$="fn*" THEN GOTO EVAL_FN
    GOTO EVAL_INVOKE

    EVAL_GET_A3:
      A3=Z%(Z%(Z%(A,1),1),1)+1
      R=A3:GOSUB DEREF_R:A3=R
    EVAL_GET_A2:
      A2=Z%(Z%(A,1),1)+1
      R=A2:GOSUB DEREF_R:A2=R
    EVAL_GET_A1:
      A1=Z%(A,1)+1
      R=A1:GOSUB DEREF_R:A1=R
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
        A=Z%(A1,1)+1:CALL EVAL
        GOSUB POP_Q:A1=Q: REM pop A1

        IF ER<>-2 THEN GOTO EVAL_LET_LOOP_DONE

        REM set environment: even A1 key to odd A1 eval'd above
        K=A1+1:C=R:GOSUB ENV_SET
        AY=R:GOSUB RELEASE: REM release our use, ENV_SET took ownership

        REM skip to the next pair of A1 elements
        A1=Z%(Z%(A1,1),1)
        GOTO EVAL_LET_LOOP

      EVAL_LET_LOOP_DONE:
        GOSUB POP_Q:A2=Q: REM pop A2
        A=A2:CALL EVAL: REM eval A2 using let_env
        GOTO EVAL_RETURN
    EVAL_DO:
      A=Z%(A,1): REM rest

      CALL EVAL_AST

      GOSUB PUSH_R: REM push eval'd list
      A=R:GOSUB LAST: REM return the last element
      GOSUB POP_Q:AY=Q: REM pop eval'd list
      GOSUB RELEASE: REM release the eval'd list
      GOTO EVAL_RETURN

    EVAL_IF:
      GOSUB EVAL_GET_A1: REM set A1
      GOSUB PUSH_A: REM push/save A
      A=A1:CALL EVAL
      GOSUB POP_A: REM pop/restore A
      IF (R=0) OR (R=1) THEN GOTO EVAL_IF_FALSE

      EVAL_IF_TRUE:
        AY=R:GOSUB RELEASE
        GOSUB EVAL_GET_A2: REM set A1 and A2 after EVAL
        A=A2:GOTO EVAL_TCO_RECUR: REM TCO loop
      EVAL_IF_FALSE:
        AY=R:GOSUB RELEASE
        REM if no false case (A3), return nil
        GOSUB COUNT
        IF R<4 THEN R=0:GOTO EVAL_RETURN
        GOSUB EVAL_GET_A3: REM set A1 - A3 after EVAL
        A=A3:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_FN:
      GOSUB EVAL_GET_A2: REM set A1 and A2
      A=A2:B=A1:GOSUB MAL_FUNCTION
      GOTO EVAL_RETURN

    EVAL_INVOKE:
      CALL EVAL_AST

      REM if error, return f/args for release by caller
      IF ER<>-2 THEN GOTO EVAL_RETURN

      REM push f/args for release after call
      GOSUB PUSH_R

      F=R+1

      AR=Z%(R,1): REM rest
      R=F:GOSUB DEREF_R:F=R

      REM if metadata, get the actual object
      IF (Z%(F,0)AND 31)>=16 THEN F=Z%(F,1)

      IF (Z%(F,0)AND 31)=9 THEN GOTO EVAL_DO_FUNCTION
      IF (Z%(F,0)AND 31)=10 THEN GOTO EVAL_DO_MAL_FUNCTION

      REM if error, pop and return f/args for release by caller
      GOSUB POP_R
      ER=-1:E$="apply of non-function":GOTO EVAL_RETURN

      EVAL_DO_FUNCTION:
        REM regular function
        IF Z%(F,1)<60 THEN GOSUB DO_FUNCTION:GOTO EVAL_DO_FUNCTION_SKIP
        REM for recur functions (apply, map, swap!), use GOTO
        IF Z%(F,1)>60 THEN CALL DO_TCO_FUNCTION
        EVAL_DO_FUNCTION_SKIP:

        REM pop and release f/args
        GOSUB POP_Q:AY=Q
        GOSUB RELEASE
        GOTO EVAL_RETURN

      EVAL_DO_MAL_FUNCTION:
        E4=E: REM save the current environment for release

        REM create new environ using env stored with function
        C=Z%(F+1,1):A=Z%(F+1,0):B=AR:GOSUB ENV_NEW_BINDS

        REM release previous env if it is not the top one on the
        REM stack (X%(X-2)) because our new env refers to it and
        REM we no longer need to track it (since we are TCO recurring)
        GOSUB PEEK_Q_2
        IF E4<>Q THEN AY=E4:GOSUB RELEASE

        REM claim the AST before releasing the list containing it
        A=Z%(F,1):Z%(A,0)=Z%(A,0)+32
        REM add AST to pending release queue to free as soon as EVAL
        REM actually returns (LV+1)
        LV=LV+1:GOSUB PEND_A_LV:LV=LV-1

        REM pop and release f/args
        GOSUB POP_Q:AY=Q
        GOSUB RELEASE

        REM A set above
        E=R:GOTO EVAL_TCO_RECUR: REM TCO loop

  EVAL_RETURN:
    REM AZ=R: B=1: GOSUB PR_STR
    REM PRINT "EVAL_RETURN R: ["+R$+"] ("+STR$(R)+"), LV:"+STR$(LV)+",ER:"+STR$(ER)

    REM release environment if not the top one on the stack
    GOSUB PEEK_Q_1
    IF E<>Q THEN AY=E:GOSUB RELEASE

    LV=LV-1: REM track basic return stack level

    REM release everything we couldn't release earlier
    GOSUB RELEASE_PEND

    REM trigger GC
    #cbm T=FRE(0)
    #qbasic T=0

    REM pop A and E off the stack
    GOSUB POP_A
    GOSUB POP_Q:E=Q

END SUB

REM PRINT(A) -> R$
MAL_PRINT:
  AZ=A:B=1:GOSUB PR_STR
  RETURN

REM RE(A$) -> R
REM Assume D has repl_env
REM caller must release result
RE:
  R1=0
  GOSUB MAL_READ
  R1=R
  IF ER<>-2 THEN GOTO RE_DONE

  A=R:E=D:CALL EVAL

  RE_DONE:
    REM Release memory from MAL_READ
    IF R1<>0 THEN AY=R1:GOSUB RELEASE
    RETURN: REM caller must release result of EVAL

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
  C=-1:GOSUB ENV_NEW:D=R

  REM core.EXT: defined in Basic
  E=D:GOSUB INIT_CORE_NS: REM set core functions in repl_env

  ZT=ZI: REM top of memory after base repl_env

  REM core.mal: defined using the language itself
  A$="(def! not (fn* (a) (if a false true)))"
  GOSUB RE:AY=R:GOSUB RELEASE

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

