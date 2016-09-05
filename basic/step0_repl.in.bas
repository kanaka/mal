GOTO MAIN

REM $INCLUDE: 'readline.in.bas'

REM /* READ(A$) -> R$ */
MAL_READ:
  R$=A$
  RETURN

REM /* EVAL(A$, E%) -> R$ */
EVAL:
  GOSUB MAL_READ: REM /* call READ */
  RETURN

REM /* PRINT(A$) -> R$ */
MAL_PRINT:
  GOSUB EVAL: REM /* call EVAL */
  RETURN

REM /* REP(A$) -> R$ */
REP:
  GOSUB MAL_PRINT: REM /* call PRINT */
  PRINT R$
  RETURN

REM /* main program loop */
MAIN:
  A$="user> "
  GOSUB READLINE: REM /* call input parser */
  IF EOF=1 THEN END
  A$=R$
  GOSUB REP: REM /* call REP */
  GOTO MAIN

