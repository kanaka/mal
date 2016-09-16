REM PR_STR(AZ%, PR%) -> R$
PR_STR:
  RR$=""
  PR_STR_RECUR:
  T%=Z%(AZ%,0)
  REM PRINT "AZ%: " + STR$(AZ%) + ", T%: " + STR$(T%) + ", V%: " + STR$(Z%(AZ%,1))
  IF T%=15 THEN AZ%=Z%(AZ%,1): GOTO PR_STR_RECUR
  IF T%=0 THEN R$="nil": RETURN
  IF (T%=1) AND (Z%(AZ%,1)=0) THEN R$="false": RETURN
  IF (T%=1) AND (Z%(AZ%,1)=1) THEN R$="true": RETURN
  IF T%=2 THEN PR_INTEGER
  IF (T%=4) AND (PR%=0) THEN PR_STRING
  IF (T%=4) AND (PR%=1) THEN PR_STRING_READABLY
  IF T%=5 THEN PR_SYMBOL
  IF T%=6 THEN PR_SEQ
  IF T%=7 THEN PR_SEQ
  IF T%=8 THEN PR_SEQ
  IF T%=9 THEN PR_FUNCTION
  IF T%=10 THEN PR_MAL_FUNCTION
  R$="#<unknown>"
  RETURN

  PR_INTEGER:
    T5%=Z%(AZ%,1)
    R$=STR$(T5%)
    IF T5%<0 THEN RETURN
    REM Remove initial space
    R$=RIGHT$(R$, LEN(R$)-1)
    RETURN
  PR_STRING:
    R$=ZS$(Z%(AZ%,1))
    RETURN
  PR_STRING_READABLY:
    R$=CHR$(34) + ZS$(Z%(AZ%,1)) + CHR$(34)
    RETURN
  PR_SYMBOL:
    R$=ZS$(Z%(AZ%,1))
    RETURN
  PR_SEQ:
    IF T%=6 THEN RR$=RR$+"("
    IF T%=7 THEN RR$=RR$+"["
    IF T%=8 THEN RR$=RR$+"{"
    REM push where we are in the sequence
    ZL%=ZL%+1
    ZZ%(ZL%)= AZ%
    PR_SEQ_LOOP:
      IF Z%(AZ%,1) = 0 THEN PR_SEQ_DONE
      AZ%=AZ%+1
      REM Push type we are rendering on the stack
      ZL%=ZL%+1
      ZZ%(ZL%) = Z%(AZ%,0)
      GOSUB PR_STR_RECUR
      REM if we just rendered a non-sequence, then append it
      IF (T% < 6) OR (T% > 8) THEN RR$=RR$+R$
      REM pop type off stack and check it
      T%=ZZ%(ZL%)
      ZL%=ZL%-1
      REM Go to next list element
      AZ%=Z%(ZZ%(ZL%),1)
      ZZ%(ZL%) = AZ%
      IF Z%(AZ%,1) <> 0 THEN RR$=RR$+" "
      GOTO PR_SEQ_LOOP
    PR_SEQ_DONE:
      REM get current type
      T%=Z%(ZZ%(ZL%),0)
      REM pop where we are the sequence
      ZL%=ZL%-1
      IF T%=6 THEN RR$=RR$+")"
      IF T%=7 THEN RR$=RR$+"]"
      IF T%=8 THEN RR$=RR$+"}"
      R$=RR$
      RETURN
  PR_FUNCTION:
    T1%=Z%(AZ%,1)
    R$="#<function" + STR$(T1%) + ">"
    RETURN
  PR_MAL_FUNCTION:
    T1%=AZ%
    AZ%=Z%(T1%+1,0): GOSUB PR_STR_RECUR
    T7$="(fn* " + R$
    AZ%=Z%(T1%,1): GOSUB PR_STR_RECUR
    R$=T7$ + " " + R$ + ")"
    RETURN
    
REM PR_STR_SEQ(AZ%, PR%, SE$) -> R$
PR_STR_SEQ:
  T9%=AZ%
  R1$=""
  PR_STR_SEQ_LOOP:
    IF Z%(T9%,1)=0 THEN R$=R1$: RETURN
    AZ%=T9%+1: GOSUB PR_STR
    REM goto the next sequence element
    T9%=Z%(T9%,1)
    IF Z%(T9%,1)=0 THEN R1$=R1$+R$
    IF Z%(T9%,1)<>0 THEN R1$=R1$+R$+SE$
    GOTO PR_STR_SEQ_LOOP
