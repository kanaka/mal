REM PR_STR(AZ, B) -> R$
PR_STR:
  R$=""
  PR_STR_RECUR:
  T=Z%(AZ,0)AND 31
  U=Z%(AZ,1)
  REM PRINT "AZ: "+STR$(AZ)+", T: "+STR$(T)+", C: "+STR$(U)
  IF T=0 THEN R$="nil":RETURN
  REM if metadata, then get actual object
  IF T>=16 THEN AZ=U:GOTO PR_STR_RECUR
  ON T GOTO PR_BOOLEAN,PR_INTEGER,PR_UNKNOWN,PR_STRING_MAYBE,PR_SYMBOL,PR_SEQ,PR_SEQ,PR_SEQ,PR_FUNCTION,PR_MAL_FUNCTION,PR_MAL_FUNCTION,PR_ATOM,PR_ENV,PR_RECUR,PR_FREE

  PR_UNKNOWN:
    R$="#<unknown>"
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
    IF B=1 THEN PR_STRING_READABLY
    RETURN
  PR_STRING_READABLY:
    S1$="\":S2$="\\":GOSUB REPLACE: REM escape backslash "
    S1$=CHR$(34):S2$="\"+CHR$(34):GOSUB REPLACE: REM escape quotes "
    S1$=CHR$(13):S2$="\n":GOSUB REPLACE: REM escape newlines
    R$=CHR$(34)+R$+CHR$(34)
    RETURN
  PR_SYMBOL:
    R$=S$(U)
    RETURN
  PR_SEQ:
    REM push the type and where we are in the sequence
    X=X+2
    X%(X-1)=T
    X%(X)=AZ
    REM save the current rendered string
    S$(S)=R$:S=S+1
    PR_SEQ_LOOP:
      IF Z%(AZ,1)=0 THEN PR_SEQ_DONE
      AZ=AZ+1:GOSUB PR_STR
      REM append what we just rendered it
      S$(S-1)=S$(S-1)+R$
      REM restore current seq type
      T=X%(X-1)
      REM Go to next list element
      AZ=Z%(X%(X),1)
      X%(X)=AZ
      IF Z%(AZ,1)<>0 THEN S$(S-1)=S$(S-1)+" "
      GOTO PR_SEQ_LOOP
    PR_SEQ_DONE:
      REM restore the current string
      S=S-1:R$=S$(S)
      REM get type
      T=X%(X-1)
      REM pop where we are the sequence and type
      X=X-2
      IF T=6 THEN R$="("+R$+")"
      IF T=7 THEN R$="["+R$+"]"
      IF T=8 THEN R$="{"+R$+"}"
      RETURN
  PR_FUNCTION:
    T1=U
    R$="#<function"+STR$(T1)+">"
    RETURN
  PR_MAL_FUNCTION:
    T1=AZ
    AZ=Z%(T1+1,0):GOSUB PR_STR
    REM append what we just rendered it
    S$(S)="(fn* "+R$:S=S+1
    AZ=Z%(T1,1):GOSUB PR_STR
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
    
REM PR_STR_SEQ(AZ, B, SE$) -> R$
PR_STR_SEQ:
  T9=AZ
  S$(S)="":S=S+1
  PR_STR_SEQ_LOOP:
    IF Z%(T9,1)=0 THEN S=S-1:R$=S$(S):RETURN
    AZ=T9+1:GOSUB PR_STR
    REM goto the next sequence element
    T9=Z%(T9,1)
    IF Z%(T9,1)=0 THEN S$(S-1)=S$(S-1)+R$
    IF Z%(T9,1)<>0 THEN S$(S-1)=S$(S-1)+R$+SE$
    GOTO PR_STR_SEQ_LOOP
