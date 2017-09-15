REM PR_STR(AZ, B) -> R$
PR_STR:
  R$=""
  PR_STR_RECUR:
  T=Z%(AZ)AND 31
  U=Z%(AZ+1)
  REM PRINT "AZ: "+STR$(AZ)+", T: "+STR$(T)+", U: "+STR$(U)
  IF T=0 THEN R$="nil":RETURN
  REM if metadata, then get actual object
  IF T>=14 THEN AZ=U:GOTO PR_STR_RECUR
  ON T GOTO PR_BOOLEAN,PR_INTEGER,PR_UNKNOWN,PR_STRING_MAYBE,PR_SYMBOL,PR_SEQ,PR_SEQ,PR_SEQ,PR_FUNCTION,PR_MAL_FUNCTION,PR_MAL_FUNCTION,PR_ATOM,PR_ENV,PR_RECUR,PR_FREE

  PR_UNKNOWN:
    REM MEMORY DEBUGGING:
    REM R$="#<unknown>"
    RETURN
  PR_RECUR:
    AZ=U
    GOTO PR_STR_RECUR
  PR_BOOLEAN:
    R$="true"
    IF U=0 THEN R$="false"
    RETURN
  PR_INTEGER:
    T$=STR$(U)
    REM Remove initial space
    IF U>=0 THEN T$=RIGHT$(T$,LEN(T$)-1)
    R$=R$+T$
    RETURN
  PR_STRING_MAYBE:
    R$=S$(U)
    IF LEN(R$)=0 THEN GOTO PR_STRING
    IF MID$(R$,1,1)=CHR$(127) THEN R$=":"+MID$(R$,2,LEN(R$)-1):RETURN
  PR_STRING:
    IF B=1 THEN GOTO PR_STRING_READABLY
    RETURN
  PR_STRING_READABLY:
    S1$="\":S2$="\\":GOSUB REPLACE: REM escape backslash "
    S1$=CHR$(34):S2$="\"+CHR$(34):GOSUB REPLACE: REM escape quotes "
    #cbm S1$=CHR$(13):S2$="\n":GOSUB REPLACE: REM escape newlines
    #qbasic S1$=CHR$(10):S2$="\n":GOSUB REPLACE: REM escape newlines
    R$=CHR$(34)+R$+CHR$(34)
    RETURN
  PR_SYMBOL:
    R$=S$(U)
    RETURN
  PR_SEQ:
    REM push the type and where we are in the sequence
    Q=T:GOSUB PUSH_Q
    Q=AZ:GOSUB PUSH_Q
    REM save the current rendered string
    S$(S)=R$:S=S+1
    PR_SEQ_LOOP:
      IF Z%(AZ+1)=0 THEN GOTO PR_SEQ_DONE
      AZ=Z%(AZ+2):GOSUB PR_STR:GOSUB PEEK_Q_1:T=Q
      REM append what we just rendered it
      S$(S-1)=S$(S-1)+R$

      REM if this is a hash-map, print the next element
      IF T=8 THEN GOSUB PEEK_Q:AZ=Z%(Q+3):GOSUB PR_STR:S$(S-1)=S$(S-1)+" "+R$

      REM restore current seq type
      GOSUB PEEK_Q_1:T=Q
      REM Go to next list element
      GOSUB PEEK_Q
      AZ=Z%(Q+1)
      Q=AZ:GOSUB PUT_Q
      IF Z%(AZ+1)<>0 THEN S$(S-1)=S$(S-1)+" "
      GOTO PR_SEQ_LOOP
    PR_SEQ_DONE:
      REM restore the current string
      S=S-1:R$=S$(S)
      REM pop where we are the sequence and type
      GOSUB POP_Q
      GOSUB POP_Q:T=Q: REM get type
      IF T=6 THEN R$="("+R$+")"
      IF T=7 THEN R$="["+R$+"]"
      IF T=8 THEN R$="{"+R$+"}"
      RETURN
  PR_FUNCTION:
    R$="#<fn"+STR$(U)+">"
    RETURN
  PR_MAL_FUNCTION:
    T1=AZ
    AZ=Z%(T1+2):GOSUB PR_STR
    REM append what we just rendered it
    S$(S)="(fn* "+R$:S=S+1
    AZ=Z%(T1+1):GOSUB PR_STR
    S=S-1
    R$=S$(S)+" "+R$+")"
    RETURN
  PR_ATOM:
    AZ=U:GOSUB PR_STR
    R$="(atom "+R$+")"
    RETURN
  PR_ENV:
    R$="#<env"+STR$(AZ)+", data"+STR$(U)+">"
    RETURN
  PR_FREE:
    R$="#<free"+STR$(AZ)+", next"+STR$(U)+">"
    RETURN
    
REM PR_STR_SEQ(AZ, B, B$) -> R$
REM   - B is print_readably
REM   - B$ is the separator
PR_STR_SEQ:
  V=AZ
  S$(S)="":S=S+1
  PR_STR_SEQ_LOOP:
    IF Z%(V+1)=0 THEN S=S-1:R$=S$(S):RETURN
    AZ=Z%(V+2):GOSUB PR_STR
    REM goto the next sequence element
    V=Z%(V+1)
    IF Z%(V+1)=0 THEN S$(S-1)=S$(S-1)+R$
    IF Z%(V+1)<>0 THEN S$(S-1)=S$(S-1)+R$+B$
    GOTO PR_STR_SEQ_LOOP
