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

REM QUASIQUOTE(A) -> R
SUB QUASIQUOTE
  REM pair?
  IF (Z%(A,0)AND 31)<6 OR (Z%(A,0)AND 31)>7 THEN GOTO QQ_QUOTE
  IF (Z%(A,1)=0) THEN GOTO QQ_QUOTE
  GOTO QQ_UNQUOTE

  QQ_QUOTE:
    REM ['quote, ast]
    B$="quote":T=5:GOSUB STRING
    B=R:A=A:GOSUB LIST2
    AY=B:GOSUB RELEASE

    GOTO QQ_DONE

  QQ_UNQUOTE:
    R=A+1:GOSUB DEREF_R
    IF (Z%(R,0)AND 31)<>5 THEN GOTO QQ_SPLICE_UNQUOTE
    IF S$(Z%(R,1))<>"unquote" THEN GOTO QQ_SPLICE_UNQUOTE
      REM [ast[1]]
      R=Z%(A,1)+1:GOSUB DEREF_R
      Z%(R,0)=Z%(R,0)+32

      GOTO QQ_DONE

  QQ_SPLICE_UNQUOTE:
    GOSUB PUSH_A
    REM rest of cases call quasiquote on ast[1..]
    A=Z%(A,1):CALL QUASIQUOTE
    W=R
    GOSUB POP_A

    REM set A to ast[0] for last two cases
    A=A+1:GOSUB DEREF_A

    REM pair?
    IF (Z%(A,0)AND 31)<6 OR (Z%(A,0)AND 31)>7 THEN GOTO QQ_DEFAULT
    IF (Z%(A,1)=0) THEN GOTO QQ_DEFAULT

    B=A+1:GOSUB DEREF_B
    IF (Z%(B,0)AND 31)<>5 THEN GOTO QQ_DEFAULT
    IF S$(Z%(B,1))<>"splice-unquote" THEN QQ_DEFAULT
      REM ['concat, ast[0][1], quasiquote(ast[1..])]

      B=Z%(A,1)+1:GOSUB DEREF_B:B=B
      B$="concat":T=5:GOSUB STRING:C=R
      A=W:GOSUB LIST3
      REM release inner quasiquoted since outer list takes ownership
      AY=A:GOSUB RELEASE
      AY=C:GOSUB RELEASE
      GOTO QQ_DONE

  QQ_DEFAULT:
    REM ['cons, quasiquote(ast[0]), quasiquote(ast[1..])]

    Q=W:GOSUB PUSH_Q
    REM A set above to ast[0]
    CALL QUASIQUOTE
    B=R
    GOSUB POP_Q:W=Q

    B$="cons":T=5:GOSUB STRING:C=R
    A=W:GOSUB LIST3
    REM release inner quasiquoted since outer list takes ownership
    AY=A:GOSUB RELEASE
    AY=B:GOSUB RELEASE
    AY=C:GOSUB RELEASE
  QQ_DONE:
END SUB

REM MACROEXPAND(A, E) -> A:
SUB MACROEXPAND
  GOSUB PUSH_A

  MACROEXPAND_LOOP:
    REM list?
    IF (Z%(A,0)AND 31)<>6 THEN GOTO MACROEXPAND_DONE
    REM non-empty?
    IF Z%(A,1)=0 THEN GOTO MACROEXPAND_DONE
    B=A+1:GOSUB DEREF_B
    REM symbol? in first position
    IF (Z%(B,0)AND 31)<>5 THEN GOTO MACROEXPAND_DONE
    REM defined in environment?
    K=B:CALL ENV_FIND
    IF R=-1 THEN GOTO MACROEXPAND_DONE
    B=R4:GOSUB DEREF_B
    REM macro?
    IF (Z%(B,0)AND 31)<>11 THEN GOTO MACROEXPAND_DONE

    F=B:AR=Z%(A,1):CALL APPLY
    A=R

    GOSUB PEEK_Q:AY=Q
    REM if previous A was not the first A into macroexpand (i.e. an
    REM intermediate form) then free it
    IF A<>AY THEN GOSUB PEND_A_LV

    IF ER<>-2 THEN GOTO MACROEXPAND_DONE
    GOTO MACROEXPAND_LOOP

  MACROEXPAND_DONE:
    GOSUB POP_Q: REM pop original A
END SUB

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

      REM if we are returning to DO, then skip last element
      Q=6:GOSUB PEEK_Q_Q
      IF Q=2 AND Z%(Z%(A,1),1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

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
  EVAL_NOT_LIST:
  REM ELSE
    CALL EVAL_AST
    GOTO EVAL_RETURN

  APPLY_LIST:
    CALL MACROEXPAND

    GOSUB LIST_Q
    IF R<>1 THEN GOTO EVAL_NOT_LIST

    GOSUB EMPTY_Q
    IF R THEN R=A:Z%(R,0)=Z%(R,0)+32:GOTO EVAL_RETURN

    A0=A+1
    R=A0:GOSUB DEREF_R:A0=R

    REM get symbol in A$
    IF (Z%(A0,0)AND 31)<>5 THEN A$=""
    IF (Z%(A0,0)AND 31)=5 THEN A$=S$(Z%(A0,1))

    IF A$="def!" THEN GOTO EVAL_DEF
    IF A$="let*" THEN GOTO EVAL_LET
    IF A$="quote" THEN GOTO EVAL_QUOTE
    IF A$="quasiquote" THEN GOTO EVAL_QUASIQUOTE
    IF A$="defmacro!" THEN GOTO EVAL_DEFMACRO
    IF A$="macroexpand" THEN GOTO EVAL_MACROEXPAND
    IF A$="try*" THEN GOTO EVAL_TRY
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
      Q=E:GOSUB PUSH_Q: REM push env for for later release

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
        GOSUB POP_Q:AY=Q: REM pop previous env

        REM release previous environment if not the current EVAL env
        GOSUB PEEK_Q_2
        IF AY<>Q THEN GOSUB RELEASE

        GOSUB POP_Q:A2=Q: REM pop A2
        A=A2:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_DO:
      A=Z%(A,1): REM rest
      GOSUB PUSH_A: REM push/save A

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
      R=Z%(A,1)+1:GOSUB DEREF_R
      Z%(R,0)=Z%(R,0)+32
      GOTO EVAL_RETURN

    EVAL_QUASIQUOTE:
      R=Z%(A,1)+1:GOSUB DEREF_R
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
      Z%(R,0)=Z%(R,0)+1

      REM set A1 in env to A2
      K=A1:C=R:GOSUB ENV_SET
      GOTO EVAL_RETURN

    EVAL_MACROEXPAND:
      REM PRINT "macroexpand"
      R=Z%(A,1)+1:GOSUB DEREF_R
      A=R:CALL MACROEXPAND
      R=A

      REM since we are returning it unevaluated, inc the ref cnt
      Z%(R,0)=Z%(R,0)+32
      GOTO EVAL_RETURN

    EVAL_TRY:
      REM PRINT "try*"
      GOSUB EVAL_GET_A1: REM set A1, A2, and A3

      GOSUB PUSH_A: REM push/save A
      A=A1:CALL EVAL: REM eval A1
      GOSUB POP_A: REM pop/restore A

      REM if there is not error or catch block then return
      IF ER=-2 OR Z%(A,1)=0 THEN GOTO EVAL_RETURN

      REM create environment for the catch block eval
      C=E:GOSUB ENV_NEW:E=R

      GOSUB EVAL_GET_A2: REM set A1 and A2
      A=A2:GOSUB EVAL_GET_A2: REM set A1 and A2 from catch block

      REM create object for ER=-1 type raw string errors
      IF ER=-1 THEN B$=E$:T=4:GOSUB STRING:ER=R:Z%(R,0)=Z%(R,0)+32

      REM bind the catch symbol to the error object
      K=A1:C=ER:GOSUB ENV_SET
      AY=R:GOSUB RELEASE: REM release our use, env took ownership

      REM unset error for catch eval
      ER=-2:E$=""

      A=A2:CALL EVAL

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
        Q=E:GOSUB PUSH_Q: REM save the current environment for release

        REM create new environ using env stored with function
        C=Z%(F+1,1):A=Z%(F+1,0):B=AR:GOSUB ENV_NEW_BINDS

        REM release previous env if it is not the top one on the
        REM stack (X%(X-2)) because our new env refers to it and
        REM we no longer need to track it (since we are TCO recurring)
        GOSUB POP_Q:AY=Q
        GOSUB PEEK_Q_2
        IF AY<>Q THEN GOSUB RELEASE

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

  REP_DONE:
    REM Release memory from MAL_READ and EVAL
    IF R2<>0 THEN AY=R2:GOSUB RELEASE
    IF R1<>0 THEN AY=R1:GOSUB RELEASE
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
  #cbm A$="(def! *host-language* "+CHR$(34)+"C64 BASIC"+CHR$(34)+")"
  #qbasic A$="(def! *host-language* "+CHR$(34)+"QBasic"+CHR$(34)+")"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(def! not (fn* (a) (if a false true)))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(def! load-file (fn* (f) (eval (read-file f))))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs)"
  A$=A$+" (if (> (count xs) 1) (nth xs 1) (throw "+CHR$(34)+"odd number of"
  A$=A$+" forms to cond"+CHR$(34)+")) (cons 'cond (rest (rest xs)))))))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(def! *gensym-counter* (atom 0))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(def! gensym (fn* [] (symbol (str "+CHR$(34)+"G__"+CHR$(34)
  A$=A$+" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs)"
  A$=A$+" (let* (c (gensym)) `(let* (~c ~(first xs))"
  A$=A$+" (if ~c ~c (or ~@(rest xs)))))))))"
  GOSUB RE:AY=R:GOSUB RELEASE

  REM load the args file
  A$="(def! -*ARGS*- (load-file "+CHR$(34)+".args.mal"+CHR$(34)+"))"
  GOSUB RE:AY=R:GOSUB RELEASE

  REM set the argument list
  A$="(def! *ARGV* (rest -*ARGS*-))"
  GOSUB RE:AY=R:GOSUB RELEASE

  REM get the first argument
  A$="(first -*ARGS*-)"
  GOSUB RE

  REM if there is an argument, then run it as a program
  IF R<>0 THEN AY=R:GOSUB RELEASE:GOTO RUN_PROG
  REM no arguments, start REPL loop
  IF R=0 THEN GOTO REPL

  RUN_PROG:
    REM run a single mal program and exit
    A$="(load-file (first -*ARGS*-))"
    GOSUB RE
    IF ER<>-2 THEN GOSUB PRINT_ERROR
    GOTO QUIT

  REPL:
    REM print the REPL startup header
    REM save memory by printing this directly
    #cbm PRINT "Mal [C64 BASIC]"
    #qbasic PRINT "Mal [C64 QBasic]"

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT

    A$=R$:CALL REP: REM call REP

    IF ER<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    GOSUB PR_MEMORY_SUMMARY
    END

  PRINT_ERROR:
    REM if the error is an object, then print and free it
    IF ER>=0 THEN AZ=ER:B=0:GOSUB PR_STR:E$=R$:AY=ER:GOSUB RELEASE
    PRINT "Error: "+E$
    ER=-2:E$=""
    RETURN

