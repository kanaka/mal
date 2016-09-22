GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ(A$) -> R%
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL(A%, E%) -> R%
EVAL:
  R%=A%
  RETURN

REM PRINT(A%) -> R$
MAL_PRINT:
  AZ%=A%: PR%=1: GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
REP:
  GOSUB MAL_READ
  IF ER%<>0 THEN GOTO REP_DONE

  A%=R%: GOSUB EVAL
  IF ER%<>0 THEN GOTO REP_DONE

  A%=R%: GOSUB MAL_PRINT
  RT$=R$

  REP_DONE:
    REM Release memory from EVAL
    AY%=R%: GOSUB RELEASE
    R$=RT$
    RETURN

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  ZT%=ZI%: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ": GOSUB READLINE: REM call input parser
    IF EOF=1 THEN GOTO QUIT

    A$=R$: GOSUB REP: REM call REP

    IF ER%<>0 THEN GOSUB PRINT_ERROR: GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM P1%=ZT%: P2%=-1: GOSUB PR_MEMORY
    GOSUB PR_MEMORY_SUMMARY
    END

  PRINT_ERROR:
    PRINT "Error: "+ER$
    ER%=0: ER$=""
    RETURN

