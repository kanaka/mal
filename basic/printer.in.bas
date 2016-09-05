REM PR_STR(A%) -> R$
PR_STR:
  T%=ZT%(A%)
  REM PRINT "A%: " + STR$(A%) + ", T%: " + STR$(T%)
  IF T%=0 THEN R$="nil": RETURN
  IF T%=1 THEN R$="false": RETURN
  IF T%=2 THEN R$="true": RETURN
  IF T%=3 THEN PR_INTEGER
  IF T%=5 THEN PR_STRING
  IF T%=6 THEN PR_KEYWORD
  IF T%=7 THEN PR_SYMBOL
  IF T%=8 THEN PR_LIST
  R$="#<unknown>"
  RETURN

  PR_INTEGER:
    T%=ZV%(A%)
    R$=STR$(T%)
    IF T%<0 THEN RETURN
    REM Remove initial space
    R$=RIGHT$(R$, LEN(R$)-1)
    RETURN
  PR_STRING:
    R$=CHR$(34) + ZS$(ZV%(A%)) + CHR$(34)
    RETURN
  PR_KEYWORD:
    R$=":keyword"
    RETURN
  PR_SYMBOL:
    R$=ZS$(ZV%(A%))
    RETURN
  PR_LIST:
    IF PT%=-1 THEN RR$=""
    RR$=RR$+"("
    REM keep track of where we are in the list
    PT%=PT%+1
    PS%(PT%)= A%
    PR_LIST_LOOP:
      IF ZV%(A%) = 0 THEN PR_LIST_DONE
      A%=A%+1
      REM Push whether we are rendering a list on stack
      PT%=PT%+1
      IF ZT%(A%) = 8 THEN PS%(PT%) = 1
      IF ZT%(A%) <> 8 THEN PS%(PT%) = 0
      GOSUB PR_STR
      REM check append then pop off stack
      IF PS%(PT%) = 1 THEN RR$=RR$
      IF PS%(PT%) = 0 THEN RR$=RR$+R$
      PT%=PT%-1
      REM Go to next list element
      A%=ZV%(PS%(PT%))
      PS%(PT%) = A%
      IF ZV%(A%) <> 0 THEN RR$=RR$+" "
      GOTO PR_LIST_LOOP
    PR_LIST_DONE:
      PT%=PT%-1
      RR$=RR$+")"
      IF PT%=-1 THEN R$=RR$
      RETURN


PR_MEMORY:
  PRINT "Memory:"
  FOR I=0 TO ZI%-1
    PRINT " " + STR$(I) + ": type: " + STR$(ZT%(I)) + ", value: " + STR$(ZV%(I))
    NEXT I
  RETURN
