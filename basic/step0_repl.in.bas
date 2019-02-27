GOTO MAIN

REM $INCLUDE: 'readline.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ(A$) -> R$
MAL_READ:
  R$=A$
  RETURN

REM EVAL(A$, E) -> R$
EVAL:
  R$=A$
  RETURN

REM PRINT(A$) -> R$
MAL_PRINT:
  R$=A$
  RETURN

REM REP(A$) -> R$
REP:
  GOSUB MAL_READ
  A=R:GOSUB EVAL
  A=R:GOSUB MAL_PRINT
  RETURN

REM MAIN program
MAIN:
  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT

    A$=R$:GOSUB REP: REM call REP

    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM GOSUB PR_MEMORY_SUMMARY_SMALL
    #cbm END
    #qbasic SYSTEM

