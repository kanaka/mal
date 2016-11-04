REM POKE 1, PEEK(1)AND248: REM enable all ROM areas as RAM
REM POKE 55,0: POKE 56,192: CLR: REM move BASIC end from $A000 to $C000
GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
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
    REM push A on the stack
    X=X+1:X%(X)=A
    REM rest of cases call quasiquote on ast[1..]
    A=Z%(A,1):CALL QUASIQUOTE
    T6=R
    REM pop A off the stack
    A=X%(X):X=X-1

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
      A=T6:GOSUB LIST3
      REM release inner quasiquoted since outer list takes ownership
      AY=A:GOSUB RELEASE
      AY=C:GOSUB RELEASE
      GOTO QQ_DONE

  QQ_DEFAULT:
    REM ['cons, quasiquote(ast[0]), quasiquote(ast[1..])]

    REM push T6 on the stack
    X=X+1:X%(X)=T6
    REM A set above to ast[0]
    CALL QUASIQUOTE
    B=R
    REM pop T6 off the stack
    T6=X%(X):X=X-1

    B$="cons":T=5:GOSUB STRING:C=R
    A=T6:GOSUB LIST3
    REM release inner quasiquoted since outer list takes ownership
    AY=A:GOSUB RELEASE
    AY=B:GOSUB RELEASE
    AY=C:GOSUB RELEASE
  QQ_DONE:
END SUB

REM MACROEXPAND(A, E) -> A:
SUB MACROEXPAND
  REM push original A
  X=X+1:X%(X)=A

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
    B=T4:GOSUB DEREF_B
    REM macro?
    IF (Z%(B,0)AND 31)<>11 THEN GOTO MACROEXPAND_DONE
  
    F=B:AR=Z%(A,1):CALL APPLY
    A=R

    AY=X%(X)
    REM if previous A was not the first A into macroexpand (i.e. an
    REM intermediate form) then free it
    IF A<>AY THEN Y=Y+1:Y%(Y,0)=A:Y%(Y,1)=LV
  
    IF ER<>-2 THEN GOTO MACROEXPAND_DONE
    GOTO MACROEXPAND_LOOP

  MACROEXPAND_DONE:
    X=X-1: REM pop original A
END SUB

REM EVAL_AST(A, E) -> R
SUB EVAL_AST
  REM push A and E on the stack
  X=X+2:X%(X-1)=E:X%(X)=A

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

      REM if we are returning to DO, then skip last element
      IF X%(X-6)=2 AND Z%(Z%(A,1),1)=0 THEN GOTO EVAL_AST_SEQ_LOOP_DONE

      REM if hashmap, skip eval of even entries (keys)
      IF (X%(X-3)=8) AND ((X%(X-2)AND 1)=0) THEN GOTO EVAL_AST_DO_REF
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
      REM if no error, get return value (new seq)
      IF ER=-2 THEN R=X%(X-1)
      REM otherwise, free the return value and return nil
      IF ER<>-2 THEN R=0:AY=X%(X-1):GOSUB RELEASE

      REM pop previous, return, index and type
      X=X-4
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    REM pop A and E off the stack
    E=X%(X-1):A=X%(X):X=X-2
END SUB

REM EVAL(A, E) -> R
SUB EVAL
  LV=LV+1: REM track basic return stack level

  REM push A and E on the stack
  X=X+2:X%(X-1)=E:X%(X)=A

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

      X=X+1:X%(X)=A1: REM push A1
      A=A2:CALL EVAL: REM eval a2
      A1=X%(X):X=X-1: REM pop A1

      IF ER<>-2 THEN GOTO EVAL_RETURN

      REM set a1 in env to a2
      K=A1:C=R:GOSUB ENV_SET
      GOTO EVAL_RETURN

    EVAL_LET:
      REM PRINT "let*"
      GOSUB EVAL_GET_A2: REM set A1 and A2

      X=X+1:X%(X)=A2: REM push/save A2
      X=X+1:X%(X)=E: REM push env for for later release

      REM create new environment with outer as current environment
      C=E:GOSUB ENV_NEW
      E=R
      EVAL_LET_LOOP:
        IF Z%(A1,1)=0 THEN GOTO EVAL_LET_LOOP_DONE

        X=X+1:X%(X)=A1: REM push A1
        REM eval current A1 odd element
        A=Z%(A1,1)+1:CALL EVAL
        A1=X%(X):X=X-1: REM pop A1

        IF ER<>-2 THEN GOTO EVAL_LET_LOOP_DONE

        REM set environment: even A1 key to odd A1 eval'd above
        K=A1+1:C=R:GOSUB ENV_SET
        AY=R:GOSUB RELEASE: REM release our use, ENV_SET took ownership

        REM skip to the next pair of A1 elements
        A1=Z%(Z%(A1,1),1)
        GOTO EVAL_LET_LOOP

      EVAL_LET_LOOP_DONE:
        E4=X%(X):X=X-1: REM pop previous env

        REM release previous environment if not the current EVAL env
        IF E4<>X%(X-2) THEN AY=E4:GOSUB RELEASE

        A2=X%(X):X=X-1: REM pop A2
        A=A2:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_DO:
      A=Z%(A,1): REM rest
      X=X+1:X%(X)=A: REM push/save A

      CALL EVAL_AST

      REM cleanup
      AY=R: REM get eval'd list for release

      A=X%(X):X=X-1: REM pop/restore original A for LAST
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
      REM add quasiquote result to pending release queue to free when
      REM next lower EVAL level returns (LV)
      Y=Y+1:Y%(Y,0)=R:Y%(Y,1)=LV

      A=R:GOTO EVAL_TCO_RECUR: REM TCO loop

    EVAL_DEFMACRO:
      REM PRINT "defmacro!"
      GOSUB EVAL_GET_A2: REM set A1 and A2

      X=X+1:X%(X)=A1: REM push A1
      A=A2:CALL EVAL: REM eval A2
      A1=X%(X):X=X-1: REM pop A1

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

      X=X+1:X%(X)=A: REM push/save A
      A=A1:CALL EVAL: REM eval A1
      A=X%(X):X=X-1: REM pop/restore A

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
      REM push A
      X=X+1:X%(X)=A
      A=A1:CALL EVAL
      REM pop A
      A=X%(X):X=X-1
      IF (R=0) OR (R=1) THEN GOTO EVAL_IF_FALSE

      EVAL_IF_TRUE:
        AY=R:GOSUB RELEASE
        GOSUB EVAL_GET_A2: REM set A1 and A2 after EVAL
        A=A2:GOTO EVAL_TCO_RECUR: REM TCO loop
      EVAL_IF_FALSE:
        AY=R:GOSUB RELEASE
        REM if no false case (A3), return nil
        B=A:GOSUB COUNT
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
      X=X+1:X%(X)=R

      F=R+1

      AR=Z%(R,1): REM rest
      R=F:GOSUB DEREF_R:F=R

      REM if metadata, get the actual object
      IF (Z%(F,0)AND 31)>=16 THEN F=Z%(F,1)

      IF (Z%(F,0)AND 31)=9 THEN GOTO EVAL_DO_FUNCTION
      IF (Z%(F,0)AND 31)=10 THEN GOTO EVAL_DO_MAL_FUNCTION

      REM if error, pop and return f/args for release by caller
      R=X%(X):X=X-1
      ER=-1:E$="apply of non-function":GOTO EVAL_RETURN

      EVAL_DO_FUNCTION:
        REM regular function
        IF Z%(F,1)<60 THEN GOSUB DO_FUNCTION:GOTO EVAL_DO_FUNCTION_SKIP
        REM for recur functions (apply, map, swap!), use GOTO
        IF Z%(F,1)>60 THEN CALL DO_TCO_FUNCTION
        EVAL_DO_FUNCTION_SKIP:

        REM pop and release f/args
        AY=X%(X):X=X-1:GOSUB RELEASE
        GOTO EVAL_RETURN

      EVAL_DO_MAL_FUNCTION:
        E4=E: REM save the current environment for release

        REM create new environ using env stored with function
        C=Z%(F+1,1):A=Z%(F+1,0):B=AR:GOSUB ENV_NEW_BINDS

        REM release previous env if it is not the top one on the
        REM stack (X%(X-2)) because our new env refers to it and
        REM we no longer need to track it (since we are TCO recurring)
        IF E4<>X%(X-2) THEN AY=E4:GOSUB RELEASE

        REM claim the AST before releasing the list containing it
        A=Z%(F,1):Z%(A,0)=Z%(A,0)+32
        REM add AST to pending release queue to free as soon as EVAL
        REM actually returns (LV+1)
        Y=Y+1:Y%(Y,0)=A:Y%(Y,1)=LV+1

        REM pop and release f/args
        AY=X%(X):X=X-1:GOSUB RELEASE

        REM A set above
        E=R:GOTO EVAL_TCO_RECUR: REM TCO loop

  EVAL_RETURN:
    REM AZ=R: B=1: GOSUB PR_STR
    REM PRINT "EVAL_RETURN R: ["+R$+"] ("+STR$(R)+"), LV:"+STR$(LV)+",ER:"+STR$(ER)

    REM release environment if not the top one on the stack
    IF E<>X%(X-1) THEN AY=E:GOSUB RELEASE

    LV=LV-1: REM track basic return stack level

    REM release everything we couldn't release earlier
    GOSUB RELEASE_PEND

    REM trigger GC
    #cbm T=FRE(0)
    #qbasic T=0

    REM pop A and E off the stack
    E=X%(X-1):A=X%(X):X=X-2

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

  A$="(def! load-file (fn* (f) (eval (read-file f))))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs)"
  A$=A$+" (if (> (count xs) 1) (nth xs 1) (throw "+CHR$(34)+"odd number of"
  A$=A$+" forms to cond"+CHR$(34)+")) (cons 'cond (rest (rest xs)))))))"
  GOSUB RE:AY=R:GOSUB RELEASE

  A$="(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs)"
  A$=A$+" `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"
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
  IF R=0 THEN GOTO REPL_LOOP

  RUN_PROG:
    REM run a single mal program and exit
    A$="(load-file (first -*ARGS*-))"
    GOSUB RE
    IF ER<>-2 THEN GOSUB PRINT_ERROR
    GOTO QUIT

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
    REM if the error is an object, then print and free it
    IF ER>=0 THEN AZ=ER:B=0:GOSUB PR_STR:E$=R$:AY=ER:GOSUB RELEASE
    PRINT "Error: "+E$
    ER=-2:E$=""
    RETURN

