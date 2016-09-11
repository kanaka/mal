REM PR_STR(AZ%) -> R$
PR_STR:
  T%=Z%(AZ%,0)
  REM PRINT "AZ%: " + STR$(AZ%) + ", T%: " + STR$(T%)
  IF T%=15 THEN AZ%=Z%(AZ%,1): GOTO PR_STR
  IF T%=0 THEN R$="nil": RETURN
  IF (T%=1) AND (Z%(AZ%,1)=0) THEN R$="false": RETURN
  IF (T%=1) AND (Z%(AZ%,1)=1) THEN R$="true": RETURN
  IF T%=2 THEN PR_INTEGER
  IF T%=4 THEN PR_STRING
  IF T%=5 THEN PR_SYMBOL
  IF T%=6 THEN PR_SEQ
  IF T%=8 THEN PR_SEQ
  IF T%=10 THEN PR_SEQ
  IF T%=12 THEN PR_FUNCTION
  R$="#<unknown>"
  RETURN

  PR_INTEGER:
    T%=Z%(AZ%,1)
    R$=STR$(T%)
    IF T%<0 THEN RETURN
    REM Remove initial space
    R$=RIGHT$(R$, LEN(R$)-1)
    RETURN
  PR_STRING:
    R$=CHR$(34) + ZS$(Z%(AZ%,1)) + CHR$(34)
    RETURN
  PR_SYMBOL:
    R$=ZS$(Z%(AZ%,1))
    RETURN
  PR_SEQ:
    IF PT%=-1 THEN RR$=""
    IF T%=6 THEN RR$=RR$+"("
    IF T%=8 THEN RR$=RR$+"["
    IF T%=10 THEN RR$=RR$+"{"
    REM push where we are in the sequence
    PT%=PT%+1
    PS%(PT%)= AZ%
    PR_SEQ_LOOP:
      IF Z%(AZ%,1) = 0 THEN PR_SEQ_DONE
      AZ%=AZ%+1
      REM Push type we are rendering on the stack
      PT%=PT%+1
      PS%(PT%) = Z%(AZ%,0)
      GOSUB PR_STR
      REM check type and pop off stack
      T%=PS%(PT%)
      IF (T% >= 6) AND (T% <= 11) THEN RR$=RR$
      IF (T% < 6) OR (T% > 11) THEN RR$=RR$+R$
      PT%=PT%-1
      REM Go to next list element
      AZ%=Z%(PS%(PT%),1)
      PS%(PT%) = AZ%
      IF Z%(AZ%,1) <> 0 THEN RR$=RR$+" "
      GOTO PR_SEQ_LOOP
    PR_SEQ_DONE:
      T%=Z%(PS%(PT%),0)
      PT%=PT%-1
      IF T%=6 THEN RR$=RR$+")"
      IF T%=8 THEN RR$=RR$+"]"
      IF T%=10 THEN RR$=RR$+"}"
      IF PT%=-1 THEN R$=RR$
      RETURN
  PR_FUNCTION:
    T1%=Z%(AZ%,1)
    R$="#<function" + STR$(T1%) + ">"
    RETURN
    


PR_MEMORY:
  PRINT "Value Memory (Z%):"
  FOR I=0 TO ZI%-1
    PRINT " " + STR$(I) + ": type: " + STR$(Z%(I,0)) + ", value: " + STR$(Z%(I,1))
    NEXT I
  PRINT "String Memory (ZS%):"
  FOR I=0 TO ZJ%-1
    PRINT " " + STR$(I) + ": '" + ZS$(I) + "'"
    NEXT I
  RETURN
