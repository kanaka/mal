REM PR_STR(AZ, PR) -> R$
PR_STR:
  RR$=""
  PR_STR_RECUR:
  T=Z%(AZ,0)AND 31
  REM PRINT "AZ: "+STR$(AZ)+", T: "+STR$(T)+", V: "+STR$(Z%(AZ,1))
  IF T=0 THEN R$="nil":RETURN
  REM if metadata, then get actual object
  IF T>=16 THEN AZ=Z%(AZ,1):GOTO PR_STR_RECUR
  ON T GOTO PR_BOOLEAN,PR_INTEGER,PR_UNKNOWN,PR_STRING_MAYBE,PR_SYMBOL,PR_SEQ,PR_SEQ,PR_SEQ,PR_FUNCTION,PR_MAL_FUNCTION,PR_MAL_FUNCTION,PR_ATOM,PR_ENV,PR_RECUR,PR_FREE

  PR_UNKNOWN:
    R$="#<unknown>"
    RETURN
  PR_RECUR:
    AZ=Z%(AZ,1)
    GOTO PR_STR_RECUR
  PR_BOOLEAN:
    R$="true"
    IF Z%(AZ,1)=0 THEN R$="false"
    RETURN
  PR_INTEGER:
    T5=Z%(AZ,1)
    R$=STR$(T5)
    IF T5<0 THEN RETURN
    REM Remove initial space
    R$=RIGHT$(R$,LEN(R$)-1)
    RETURN
  PR_STRING_MAYBE:
    R$=S$(Z%(AZ,1))
    IF LEN(R$)=0 THEN GOTO PR_STRING
    IF MID$(R$,1,1)=CHR$(127) THEN R$=":"+MID$(R$,2,LEN(R$)-1):RETURN
  PR_STRING:
    IF PR=1 THEN PR_STRING_READABLY
    RETURN
  PR_STRING_READABLY:
    S1$=CHR$(92):S2$=CHR$(92)+CHR$(92):GOSUB REPLACE: REM escape backslash
    S1$=CHR$(34):S2$=CHR$(92)+CHR$(34):GOSUB REPLACE: REM escape quotes
    S1$=CHR$(13):S2$=CHR$(92)+"n":GOSUB REPLACE: REM escape newlines
    R$=CHR$(34)+R$+CHR$(34)
    RETURN
  PR_SYMBOL:
    R$=S$(Z%(AZ,1))
    RETURN
  PR_SEQ:
    IF T=6 THEN RR$=RR$+"("
    IF T=7 THEN RR$=RR$+"["
    IF T=8 THEN RR$=RR$+"{"
    REM push the type and where we are in the sequence
    X=X+2
    X%(X-1)=T
    X%(X)=AZ
    PR_SEQ_LOOP:
      IF Z%(AZ,1)=0 THEN PR_SEQ_DONE
      AZ=AZ+1
      GOSUB PR_STR_RECUR
      REM if we just rendered a non-sequence, then append it
      IF T<6 OR T>8 THEN RR$=RR$+R$
      REM restore current seq type
      T=X%(X-1)
      REM Go to next list element
      AZ=Z%(X%(X),1)
      X%(X)=AZ
      IF Z%(AZ,1)<>0 THEN RR$=RR$+" "
      GOTO PR_SEQ_LOOP
    PR_SEQ_DONE:
      REM get type
      T=X%(X-1)
      REM pop where we are the sequence and type
      X=X-2
      IF T=6 THEN RR$=RR$+")"
      IF T=7 THEN RR$=RR$+"]"
      IF T=8 THEN RR$=RR$+"}"
      R$=RR$
      RETURN
  PR_FUNCTION:
    T1=Z%(AZ,1)
    R$="#<function"+STR$(T1)+">"
    RETURN
  PR_MAL_FUNCTION:
    T1=AZ
    AZ=Z%(T1+1,0):GOSUB PR_STR_RECUR
    T7$="(fn* "+R$
    RR$=""
    AZ=Z%(T1,1):GOSUB PR_STR_RECUR
    R$=T7$+" "+R$+")"
    RETURN
  PR_ATOM:
    AZ=Z%(AZ,1):GOSUB PR_STR_RECUR
    R$="(atom "+R$+")"
    RETURN
  PR_ENV:
    R$="#<env"+STR$(AZ)+", data"+STR$(Z%(AZ,1))+">"
    RETURN
  PR_FREE:
    R$="#<free memory "+STR$(AZ)+", next"+STR$(Z%(AZ,1))+">"
    RETURN
    
REM PR_STR_SEQ(AZ, PR, SE$) -> R$
PR_STR_SEQ:
  T9=AZ
  R1$=""
  PR_STR_SEQ_LOOP:
    IF Z%(T9,1)=0 THEN R$=R1$:RETURN
    AZ=T9+1:GOSUB PR_STR
    REM goto the next sequence element
    T9=Z%(T9,1)
    IF Z%(T9,1)=0 THEN R1$=R1$+R$
    IF Z%(T9,1)<>0 THEN R1$=R1$+R$+SE$
    GOTO PR_STR_SEQ_LOOP
