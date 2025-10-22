GOTO MAIN

REM $INCLUDE: 'mem.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'
REM $INCLUDE: 'env.in.bas'
REM $INCLUDE: 'core.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ is inlined in RE

REM QUASIQUOTE(A) -> R
SUB QUASIQUOTE
  GOSUB TYPE_A
  T=T-4
  IF 0<T THEN ON T GOTO QQ_SYMBOL,QQ_LIST,QQ_VECTOR,QQ_MAP

  REM Return other types unchanged.
    R=A
    GOSUB INC_REF_R
    GOTO QQ_DONE

  QQ_MAP:
  QQ_SYMBOL:
    REM Return a list containing 'quote and A.
    B$="quote":T=5:GOSUB STRING
    B=R:GOSUB LIST2
    AY=B:GOSUB RELEASE
    GOTO QQ_DONE

  QQ_VECTOR:
    REM Return a list containing 'vec and the result of QQ_FOLDR on A.
    CALL QQ_FOLDR
    A=R
    B$="vec":T=5:GOSUB STRING:B=R
    GOSUB LIST2
    AY=A:GOSUB RELEASE
    AY=B:GOSUB RELEASE
    GOTO QQ_DONE

  QQ_LIST:
    REM Check if A contains 'unquote and a form.
    IF (Z%(A+1)=0) THEN GOTO QQ_LIST_NORMAL
    R=Z%(A+2)
    IF (Z%(R)AND 31)<>5 THEN GOTO QQ_LIST_NORMAL
    IF S$(Z%(R+1))<>"unquote" THEN GOTO QQ_LIST_NORMAL

  REM Indeed.  Return a list containing 'unquote and the form.
    R=Z%(Z%(A+1)+2)
    GOSUB INC_REF_R
    GOTO QQ_DONE

  QQ_LIST_NORMAL:
    REM Normal list, process with QQ_FOLDR.
    CALL QQ_FOLDR

QQ_DONE:
END SUB

REM Quasiquote right fold (A) -> R.
REM Used for unquoted lists (GOTO), vectors (GOSUB),
REM and recursively (GOSUB).
SUB QQ_FOLDR
  IF A=0 THEN GOTO QQ_EMPTY
  IF Z%(A+1)=0 THEN GOTO QQ_EMPTY
  GOTO QQ_NOTEMPTY

  QQ_EMPTY:
    REM empty list/vector -> empty list
    R=6
    GOSUB INC_REF_R

    GOTO QQ_FOLDR_DONE

  QQ_NOTEMPTY:
    REM Execute QQ_FOLDR recursively with (rest A)
    GOSUB PUSH_A
    A=Z%(A+1):CALL QQ_FOLDR
    GOSUB POP_A

    REM Set A to elt = (first A)
    A=Z%(A+2)

    REM Quasiquote transition function:
    REM A: current element, R: accumulator -> R: new accumulator

    REM check if A is a list starting with splice-unquote
    GOSUB TYPE_A
    IF T<>6 THEN GOTO QQ_DEFAULT
    IF (Z%(A+1)=0) THEN GOTO QQ_DEFAULT
    B=Z%(A+2)
    IF (Z%(B)AND 31)<>5 THEN GOTO QQ_DEFAULT
    IF S$(Z%(B+1))<>"splice-unquote" THEN GOTO QQ_DEFAULT

    REM ('concat, A[1], R)
      B=Z%(Z%(A+1)+2)
      A=R
      B$="concat":T=5:GOSUB STRING:C=R
      GOSUB LIST3
      REM release inner quasiquoted since outer list takes ownership
      AY=A:GOSUB RELEASE
      AY=C:GOSUB RELEASE

      GOTO QQ_FOLDR_DONE

    QQ_DEFAULT:
      REM ('cons, quasiquote(A), R)
      GOSUB PUSH_R
      CALL QUASIQUOTE
      B=R
      B$="cons":T=5:GOSUB STRING:C=R
      GOSUB POP_A
      GOSUB LIST3
      REM release inner quasiquoted since outer list takes ownership
      AY=A:GOSUB RELEASE
      AY=B:GOSUB RELEASE
      AY=C:GOSUB RELEASE

QQ_FOLDR_DONE:
END SUB

REM EVAL_AST(A, E) -> R
SUB EVAL_AST
  REM push A and E on the stack
  Q=E:GOSUB PUSH_Q
  GOSUB PUSH_A

  IF ER<>-2 THEN GOTO EVAL_AST_RETURN

  GOSUB TYPE_A
  IF T<6 OR 8<T THEN R=-1:ER=-1:E$="EVAL_AST: bad type":GOTO EVAL_AST_RETURN

    REM setup the stack for the loop
    GOSUB MAP_LOOP_START

    EVAL_AST_SEQ_LOOP:
      REM check if we are done evaluating the source sequence
      IF Z%(A+1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM if we are returning to DO, then skip last element
      REM The EVAL_DO call to EVAL_AST must be call #2 for EVAL_AST to
      REM return early and for TCO to work
      Q=5:GOSUB PEEK_Q_Q
      IF Q=2 AND Z%(Z%(A+1)+1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

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

  B$="DEBUG-EVAL":CALL ENV_GET
  IF R3=0 OR R=0 OR R=2 THEN GOTO DEBUG_EVAL_DONE
    AZ=A:B=1:GOSUB PR_STR
    PRINT "EVAL: "+R$+" [A:"+STR$(A)+", LV:"+STR$(LV)+"]"
  DEBUG_EVAL_DONE:

  GOSUB TYPE_A
  T=T-4
  IF 0<T THEN ON T GOTO EVAL_SYMBOL,APPLY_LIST,EVAL_VECTOR,EVAL_MAP

  REM ELSE
    R=A
    GOSUB INC_REF_R
    GOTO EVAL_RETURN

  EVAL_SYMBOL:
    B$=S$(Z%(A+1)):CALL ENV_GET
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

    REM get symbol in A$
    IF (Z%(A0)AND 31)<>5 THEN A$=""
    IF (Z%(A0)AND 31)=5 THEN A$=S$(Z%(A0+1))

    IF A$="def!" THEN GOTO EVAL_DEF
    IF A$="let*" THEN GOTO EVAL_LET
    IF A$="quote" THEN GOTO EVAL_QUOTE
    IF A$="quasiquote" THEN GOTO EVAL_QUASIQUOTE
    IF A$="defmacro!" THEN GOTO EVAL_DEFMACRO
    IF A$="do" THEN GOTO EVAL_DO
    IF A$="if" THEN GOTO EVAL_IF
    IF A$="fn*" THEN GOTO EVAL_FN
    GOTO EVAL_INVOKE

    EVAL_GET_A3:
      A3=Z%(Z%(Z%(Z%(A+1)+1)+1)+2)
    EVAL_GET_A2:
      A2=Z%(Z%(Z%(A+1)+1)+2)
    EVAL_GET_A1:
      A1=Z%(Z%(A+1)+2)
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
      Q=E:GOSUB PUSH_Q: REM push env for for later release

      REM create new environment with outer as current environment
      C=E:GOSUB ENV_NEW
      E=R
      EVAL_LET_LOOP:
        IF Z%(A1+1)=0 THEN GOTO EVAL_LET_LOOP_DONE

        Q=A1:GOSUB PUSH_Q: REM push A1
        REM eval current A1 odd element
        A=Z%(Z%(A1+1)+2):CALL EVAL
        GOSUB POP_Q:A1=Q: REM pop A1

        IF ER<>-2 THEN GOTO EVAL_LET_LOOP_DONE

        REM set key/value in the environment
        K=Z%(A1+2):C=R:GOSUB ENV_SET
        AY=R:GOSUB RELEASE: REM release our use, ENV_SET took ownership

        REM skip to the next pair of A1 elements
        A1=Z%(Z%(A1+1)+1)
        GOTO EVAL_LET_LOOP

      EVAL_LET_LOOP_DONE:
        GOSUB POP_Q:AY=Q: REM pop previous env

        REM release previous environment if not the current EVAL env
        GOSUB PEEK_Q_2
        IF AY<>Q THEN GOSUB RELEASE

        GOSUB POP_Q:A2=Q: REM pop A2
        A=A2:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_DO:
      A=Z%(A+1): REM rest
      GOSUB PUSH_A: REM push/save A

      REM this must be EVAL_AST call #2 for EVAL_AST to return early
      REM and for TCO to work
      CALL EVAL_AST

      REM cleanup
      AY=R: REM get eval'd list for release

      GOSUB POP_A: REM pop/restore original A for LAST
      GOSUB LAST: REM get last element for return
      A=R: REM new recur AST

      REM cleanup
      GOSUB RELEASE: REM release eval'd list
      AY=A:GOSUB RELEASE: REM release LAST value (not sure why)

      GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_QUOTE:
      R=Z%(Z%(A+1)+2)
      GOSUB INC_REF_R
      GOTO EVAL_RETURN

    EVAL_QUASIQUOTE:
      R=Z%(Z%(A+1)+2)
      A=R:CALL QUASIQUOTE
      A=R
      REM add quasiquote result to pending release queue to free when
      REM next lower EVAL level returns (LV)
      GOSUB PEND_A_LV

      GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_DEFMACRO:
      REM PRINT "defmacro!"
      GOSUB EVAL_GET_A2: REM set A1 and A2

      Q=A1:GOSUB PUSH_Q: REM push A1
      A=A2:CALL EVAL: REM eval A2
      GOSUB POP_Q:A1=Q: REM pop A1

      REM change function to macro
      Z%(R)=Z%(R)+1

      REM set A1 in env to A2
      K=A1:C=R:GOSUB ENV_SET
      GOTO EVAL_RETURN

    EVAL_IF:
      GOSUB EVAL_GET_A1: REM set A1
      GOSUB PUSH_A: REM push/save A
      A=A1:CALL EVAL
      GOSUB POP_A: REM pop/restore A
      IF (R=0) OR (R=2) THEN GOTO EVAL_IF_FALSE

      EVAL_IF_TRUE:
        AY=R:GOSUB RELEASE
        GOSUB EVAL_GET_A2: REM set A1 and A2 after EVAL
        A=A2:GOTO EVAL_TCO_RECUR: REM TCO loop
      EVAL_IF_FALSE:
        AY=R:GOSUB RELEASE
        REM if no false case (A3), return nil
        GOSUB COUNT
        IF R<4 THEN R=0:GOSUB INC_REF_R:GOTO EVAL_RETURN
        GOSUB EVAL_GET_A3: REM set A1 - A3 after EVAL
        A=A3:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_FN:
      GOSUB EVAL_GET_A2: REM set A1 and A2
      T=10:L=A2:M=A1:N=E:GOSUB ALLOC: REM mal function
      GOTO EVAL_RETURN

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
      IF 0<T THEN ON T GOTO EVAL_DO_FUNCTION,EVAL_DO_MAL_FUNCTION,EVAL_MACRO

      REM if error, pop and return f for release by caller
      GOSUB POP_R
      ER=-1:E$="apply of non-function":GOTO EVAL_RETURN

      EVAL_MACRO:
        REM Apply F to the unevaluated rest of A, then free the memory for F.
        AR=Z%(A+1):CALL APPLY
        GOSUB POP_Q:AY=Q:GOSUB RELEASE
        IF ER<>-2 THEN GOTO EVAL_RETURN

        REM Evaluate the result of this macro expansion.
        A=R:GOTO EVAL_TCO_RECUR: REM TCO loop

      EVAL_DO_FUNCTION:
        REM regular function

        REM Evaluate the arguments
        A=Z%(A+1):CALL EVAL_AST
        IF ER<>-2 THEN GOSUB POP_Q:AY=Q:GOSUB RELEASE:GOTO EVAL_RETURN

        REM set F and AR, push AR (after F) in the stack for release after call
        GOSUB PEEK_Q:F=Q
        GOSUB PUSH_R
        AR=R

        IF Z%(F+1)<65 THEN GOSUB DO_FUNCTION:GOTO EVAL_DO_FUNCTION_SKIP
        REM for recur functions (apply, map, swap!), use GOTO
        IF Z%(F+1)>64 THEN CALL DO_TCO_FUNCTION
        EVAL_DO_FUNCTION_SKIP:

        REM pop and release f/args
        GOSUB POP_Q:AY=Q:GOSUB RELEASE
        GOSUB POP_Q:AY=Q
        GOSUB RELEASE
        GOTO EVAL_RETURN

      EVAL_DO_MAL_FUNCTION:

        REM Evaluate the arguments
        A=Z%(A+1):CALL EVAL_AST
        IF ER<>-2 THEN GOSUB POP_Q:AY=Q:GOSUB RELEASE:GOTO EVAL_RETURN

        REM set F and AR, push AR (after F) in the stack for release after call
        GOSUB PEEK_Q:F=Q
        GOSUB PUSH_R
        AR=R

        Q=E:GOSUB PUSH_Q: REM save the current environment for release

        REM create new environ using env and params stored in function
        C=Z%(F+3):A=Z%(F+2):B=AR:GOSUB ENV_NEW_BINDS

        REM release previous env if it is not the top one on the
        REM stack (X%(X-2)) because our new env refers to it and
        REM we no longer need to track it (since we are TCO recurring)
        GOSUB POP_Q:AY=Q
        GOSUB PEEK_Q_2
        IF AY<>Q THEN GOSUB RELEASE

        REM claim the AST before releasing the list containing it
        A=Z%(F+1):Z%(A)=Z%(A)+32
        REM add AST to pending release queue to free as soon as EVAL
        REM actually returns (LV+1)
        LV=LV+1:GOSUB PEND_A_LV:LV=LV-1

        REM pop f/args, do not release (?)
        GOSUB POP_Q
        GOSUB POP_Q

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
  C=0:GOSUB ENV_NEW:D=R

  REM core.EXT: defined in Basic
  E=D:GOSUB INIT_CORE_NS: REM set core functions in repl_env

  ZT=ZI: REM top of memory after base repl_env

  REM core.mal: defined using the language itself
  A$="(def! not (fn* (a) (if a false true)))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(def! load-file (fn* (f) (do (eval (read-file f)) nil)))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs)"
  A$=A$+" (if (> (count xs) 1) (nth xs 1) (throw "+CHR$(34)+"odd number of"
  A$=A$+" forms to cond"+CHR$(34)+")) (cons 'cond (rest (rest xs)))))))"
  GOSUB RE:AY=R:GOSUB RELEASE

  REM load the args file
  A$="(load-file "+CHR$(34)+".args.mal"+CHR$(34)+")"
  GOSUB RE:AY=R:GOSUB RELEASE

  IF ER>-2 THEN GOSUB PRINT_ERROR:END

  REM set the argument list
  A$="(def! *ARGV* (rest -*ARGS*-))"
  GOSUB RE:AY=R:GOSUB RELEASE

  REM get the first argument
  A$="(first -*ARGS*-)"
  GOSUB RE

  REM no arguments, start REPL loop
  REM if there is an argument, then run it as a program
  IF 15<R THEN GOTO RUN_PROG

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT
    IF R$="" THEN GOTO REPL_LOOP

    A$=R$:CALL REP

    IF ER<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  RUN_PROG:
    REM free up first arg because we get it again
    AY=R:GOSUB RELEASE
    REM run a single mal program and exit
    A$="(load-file (first -*ARGS*-))"
    GOSUB RE
    IF ER<>-2 THEN GOSUB PRINT_ERROR

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
