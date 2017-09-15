REM READLINE(A$) -> R$
READLINE:
  EZ=0
  PRINT A$;
  C$="":R$="":C=0
  READCH:
    #cbm GET C$
    #qbasic C$=INKEY$
    IF C$="" THEN GOTO READCH
    C=ASC(C$)
    REM PRINT C
    #qbasic IF ASC(C$)=8 THEN C=20:C$=CHR$(20)
    IF C=4 OR C=0 THEN EZ=1:GOTO RL_DONE: REM EOF
    IF C=127 OR C=20 THEN GOSUB RL_BACKSPACE
    IF C=127 OR C=20 THEN GOTO READCH
    IF (C<32 OR C>127) AND C<>13 THEN GOTO READCH
    PRINT C$;
    IF LEN(R$)<255 AND C$<>CHR$(13) THEN R$=R$+C$
    IF LEN(R$)<255 AND C$<>CHR$(13) THEN GOTO READCH
  RL_DONE:
    RETURN

  REM Assumes R$ has input buffer
  RL_BACKSPACE:
    IF LEN(R$)=0 THEN RETURN
    R$=LEFT$(R$,LEN(R$)-1)
    #cbm PRINT CHR$(157)+" "+CHR$(157);
    #qbasic LOCATE ,POS(0)-1
    #qbasic PRINT " ";
    #qbasic LOCATE ,POS(0)-1
    RETURN 
