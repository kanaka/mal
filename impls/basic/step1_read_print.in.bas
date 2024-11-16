GOTO MAIN

REM $INCLUDE: 'mem.in.bas'
REM $INCLUDE: 'types.in.bas'
REM $INCLUDE: 'readline.in.bas'
REM $INCLUDE: 'reader.in.bas'
REM $INCLUDE: 'printer.in.bas'

REM $INCLUDE: 'debug.in.bas'

REM READ is inlined in RE

REM EVAL(A) -> R
SUB EVAL
  R=A
END SUB

REM PRINT is inlined in REP

REM RE(A$) -> R
REM caller must release result
RE:
  R1=-1
  GOSUB READ_STR: REM inlined READ
  R1=R
  IF ER<>-2 THEN GOTO RE_DONE

  A=R:CALL EVAL

  RE_DONE:
    RETURN: REM caller must release result of EVAL

REM REP(A$) -> R$
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

  ZT=ZI: REM top of memory after base repl_env

  REPL_LOOP:
    A$="user> ":GOSUB READLINE: REM call input parser
    IF EZ=1 THEN GOTO QUIT
    IF R$="" THEN GOTO REPL_LOOP

    A$=R$:CALL REP

    IF ER<>-2 THEN GOSUB PRINT_ERROR:GOTO REPL_LOOP
    PRINT R$
    GOTO REPL_LOOP

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
