GOTO MAIN

REM $INCLUDE: 'mem.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ(A$) -> R
MAL_READ:
  GOSUB READ_STR
  RETURN

REM EVAL(A, E) -> R
SUB EVAL
  R=A
END SUB

REM PRINT(A) -> R$
MAL_PRINT:
  AZ=A:B=1:GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
SUB REP
  GOSUB MAL_READ
  IF ER<>-2 THEN GOTO REP_DONE

  A=R:CALL EVAL
  IF ER<>-2 THEN GOTO REP_DONE

  A=R:GOSUB MAL_PRINT

  REP_DONE:
    REM Release memory from EVAL
    AY=R:GOSUB RELEASE
END SUB

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

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
    #cbm END
    #qbasic SYSTEM

  PRINT_ERROR:
    PRINT "Error: "+E$
    ER=-2:E$=""
    RETURN

