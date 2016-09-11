GOTO MAIN

REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

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
  AZ%=A%: GOSUB PR_STR
  RETURN

REM REP(A$) -> R$
REP:
  GOSUB MAL_READ
  IF ER% THEN RETURN
  A%=R%: GOSUB EVAL
  IF ER% THEN RETURN
  A%=R%: GOSUB MAL_PRINT
  IF ER% THEN RETURN
  RETURN

REM MAIN program
MAIN:
  GOSUB INIT_MEMORY

  MAIN_LOOP:
    A$="user> "
    GOSUB READLINE: REM /* call input parser */
    IF EOF=1 THEN GOTO MAIN_DONE
    A$=R$: GOSUB REP: REM /* call REP */
    IF ER% THEN GOTO ERROR
    PRINT R$
    GOTO MAIN_LOOP

    ERROR:
      PRINT "Error: " + ER$
      ER%=0
      ER$=""
      GOTO MAIN_LOOP

  MAIN_DONE:
    PRINT "Free: " + STR$(FRE(0))
    END

