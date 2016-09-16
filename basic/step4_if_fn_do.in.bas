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
  REM PRINT "EVAL_AST: " + R$ + "(" + STR$(A%) + ")"

  GOSUB DEREF_A

  T%=Z%(A%,0)AND15
  IF T%=5 THEN EVAL_AST_SYMBOL
  IF T%=6 THEN EVAL_AST_SEQ
  IF T%=7 THEN EVAL_AST_SEQ
  IF T%=8 THEN EVAL_AST_SEQ

  REM scalar: deref to actual value and inc ref cnt
  R%=A%: GOSUB DEREF_R
  Z%(R%,0)=Z%(R%,0)+16
  GOTO EVAL_AST_RETURN

  EVAL_AST_SYMBOL:
    K%=A%: GOSUB ENV_GET
    GOTO EVAL_AST_RETURN
  
  EVAL_AST_SEQ:
    REM allocate the first entry
    SZ%=2: GOSUB ALLOC

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
        R%=A%+1: GOSUB DEREF_R: REM deref to target of referred entry
        Z%(R%,0)=Z%(R%,0)+16: REM inc ref cnt of referred value
        GOTO EVAL_AST_ADD_VALUE

      EVAL_AST_DO_EVAL:
        REM call EVAL for each entry
        A%=A%+1: GOSUB EVAL
        A%=A%-1
        IF ER%=1 THEN GOTO EVAL_AST_SEQ_LOOP_DONE
        GOSUB DEREF_R: REM deref to target of evaluated entry

      EVAL_AST_ADD_VALUE:

      REM update previous value pointer to evaluated entry 
      Z%(ZZ%(ZL%)+1,1)=R%

      REM allocate the next entry
      SZ%=2: GOSUB ALLOC

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
      IF ER%=1 THEN R%=0: AY%=ZZ%(ZL%-1): GOSUB RELEASE

      REM pop previous, return, index and type
      ZL%=ZL%-4
      GOTO EVAL_AST_RETURN

  EVAL_AST_RETURN:
    REM pop A% and E% off the stack
    E%=ZZ%(ZL%-1): A%=ZZ%(ZL%): ZL%=ZL%-2

    REM pop EVAL AST return label/address
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
  REM PRINT "EVAL: " + R$ + "(" + STR$(A%) + ")"
  REM PRINT "EVAL level: " + STR$(LV%)

  GOSUB DEREF_A

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
    IF R% THEN R%=A%: Z%(R%,0)=Z%(R%,0)+16: GOTO EVAL_RETURN

    A0%=A%+1
    R%=A0%: GOSUB DEREF_R: A0%=R%

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
      A3% = Z%(Z%(Z%(A%,1),1),1)+1
      R%=A3%: GOSUB DEREF_R: A3%=R%
    EVAL_GET_A2:
      A2% = Z%(Z%(A%,1),1)+1
      R%=A2%: GOSUB DEREF_R: A2%=R%
    EVAL_GET_A1:
      A1% = Z%(A%,1)+1
      R%=A1%: GOSUB DEREF_R: A1%=R%
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
      REM PRINT "let*"
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
        AY%=R%: GOSUB RELEASE: REM release our use, ENV_SET took ownership
        REM skip to the next pair of A1% elements
        A1%=Z%(Z%(A1%,1),1)
        GOTO EVAL_LET_LOOP
      EVAL_LET_LOOP_DONE:
        A%=A2%: GOSUB EVAL: REM eval a2 using let_env
        REM release the let env
        AY%=E%: GOSUB RELEASE
        GOTO EVAL_RETURN
    EVAL_DO:
      A%=Z%(A%,1): REM rest

      REM push EVAL_AST return label/address
      ZL%=ZL%+1: ZZ%(ZL%)=2
      GOTO EVAL_AST
      EVAL_AST_RETURN_2:

      ZM%=ZM%+1: ZR%(ZM%)=R%: REM release evaluated list
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
        AY%=R%: GOSUB RELEASE
        GOSUB EVAL_GET_A2: REM set a1% and a2% after EVAL
        A%=A2%: GOTO EVAL_TCO_RECUR
      EVAL_IF_FALSE:
        AY%=R%: GOSUB RELEASE
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
      ZM%=ZM%+1: ZR%(ZM%)=R%: REM release f/args on return

      IF ER%=1 THEN GOTO EVAL_RETURN
      F%=R%+1
      AR%=Z%(R%,1): REM rest
      R%=F%: GOSUB DEREF_R: F%=R%

      IF (Z%(F%,0)AND15)=9 THEN GOTO EVAL_DO_FUNCTION
      IF (Z%(F%,0)AND15)=10 THEN GOTO EVAL_DO_MAL_FUNCTION
      ER%=1: ER$="apply of non-function": GOTO EVAL_RETURN
      EVAL_DO_FUNCTION:
        GOSUB DO_FUNCTION
        GOTO EVAL_RETURN
      EVAL_DO_MAL_FUNCTION:
        EO%=Z%(F%+1,1): BI%=Z%(F%+1,0): EX%=AR%: GOSUB ENV_NEW_BINDS
        ZM%=ZM%+1: ZR%(ZM%)=R%: REM release environment on return
        A%=Z%(F%,1): E%=R%: GOTO EVAL_TCO_RECUR

  EVAL_RETURN:
    REM an error occured, free up any new value
    IF ER%=1 THEN AY%=R%: GOSUB RELEASE

    REM trigger GC
    TA%=FRE(0)

    REM pop A% and E% off the stack
    E%=ZZ%(ZL%-1): A%=ZZ%(ZL%): ZL%=ZL%-2

    REM AZ%=R%: GOSUB PR_STR
    REM PRINT "EVAL return: " + R$ + "(" + STR$(R%) + ")"
    REM PRINT "EVAL return level: " + STR$(LV%)

    LV%=LV%-1: REM track basic return stack level

    REM release pending queued during TCO recursion
    IF LV%=0 THEN GOSUB RELEASE_PEND

    RETURN

REM PRINT(A%) -> R$
MAL_PRINT:
  AZ%=A%: PR%=1: GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
REM Assume RE% has repl_env
REP:
  R1%=0: R2%=0
  GOSUB MAL_READ
  IF ER% THEN GOTO REP_DONE
  R1%=R%

  REM PRINT "After read:"
  REM P1%=ZT%: P2%=0: GOSUB PR_MEMORY

  A%=R%: E%=RE%: GOSUB EVAL
  IF ER% THEN GOTO REP_DONE
  R2%=R%

  REM PRINT "After eval, before print:"
  REM P1%=ZT%: P2%=0: GOSUB PR_MEMORY

  A%=R%: GOSUB MAL_PRINT

  REP_DONE:
    REM Release memory from MAL_READ and EVAL
    IF R2%<>0 THEN AY%=R2%: GOSUB RELEASE
    IF R1%<>0 THEN AY%=R1%: GOSUB RELEASE

    REM PRINT "After releases:"
    REM P1%=ZT%: P2%=0: GOSUB PR_MEMORY

    RETURN

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  LV%=0

  REM create repl_env
  EO%=-1: GOSUB ENV_NEW
  RE%=R%

  REM core.EXT: defined in Basic
  E%=RE%: GOSUB INIT_CORE_NS: REM set core functions in repl_env

  ZT%=ZI%: REM top of memory after base repl_env

  REM core.mal: defined using the language itself
  A$="(def! not (fn* (a) (if a false true)))"
  GOSUB REP

  REM AZ%=Z%(RE%,1): GOSUB PR_STR
  REM PRINT "env: " + R$ + "(" + STR$(RE%) + ")"

  REM B% = PEEK(57) + PEEK(58) * 256
  REM PRINT "57/58%: " + STR$(B%)

  MAIN_LOOP:
    A$="user> "
    GOSUB READLINE: REM /* call input parser */
    IF EOF=1 THEN GOTO MAIN_DONE
    A$=R$: GOSUB REP: REM /* call REP */

    REM P1%=ZT%: P2%=-1: GOSUB PR_MEMORY
    REM GOSUB PR_MEMORY_SUMMARY

    IF ER% THEN GOTO ERROR
    PRINT R$
    GOTO MAIN_LOOP

    ERROR:
      PRINT "Error: " + ER$
      ER%=0
      ER$=""
      GOTO MAIN_LOOP

  MAIN_DONE:
    P1%=ZT%: P2%=-1: GOSUB PR_MEMORY
    GOSUB PR_MEMORY_SUMMARY
    END

