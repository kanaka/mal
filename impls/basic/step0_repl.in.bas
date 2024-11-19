GOTO MAIN

REM $INCLUDE: 'mem.in.bas'
REM $INCLUDE: 'readline.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ is inlined in RE

REM EVAL(A$) -> R$
SUB EVAL
  R$=A$
END SUB

REM PRINT is inlined in REP

REM REP(A$) -> R$
SUB REP
  REM inlined READ (not affecting A$)
  CALL EVAL
  REM inlined PRINT (not affecting A$)
END SUB

REM MAIN program
MAIN:
  GOSUB DIM_MEMORY

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT
    IF R$="" THEN GOTO REPL_LOOP

    A$=R$:CALL REP

    PRINT R$
    GOTO REPL_LOOP

  QUIT:
    REM GOSUB PR_MEMORY_SUMMARY_SMALL
    #cbm END
    #qbasic SYSTEM
