REM READ_TOKEN(A$, IDX%) -> T$
READ_TOKEN:
  CUR%=IDX%
  REM PRINT "READ_TOKEN: " + STR$(CUR%) + ", " + MID$(A$,CUR%,1)
  T$=MID$(A$,CUR%,1)
  IF (T$="(" OR T$=")") THEN RETURN
  IF (T$="[" OR T$="]") THEN RETURN
  IF (T$="{" OR T$="}") THEN RETURN
  S1=0: S2=0: REM S1: INSTRING?, S2: ESCAPED?
  IF (T$=CHR$(34)) THEN S1=1
  CUR%=CUR%+1
  READ_TOKEN_LOOP:
    IF CUR% > LEN(A$) THEN RETURN
    CH$=MID$(A$,CUR%,1)
    IF S2 THEN GOTO READ_TOKEN_CONT
    IF S1 THEN GOTO READ_TOKEN_CONT
    IF (CH$=" " OR CH$=",") THEN RETURN
    IF (CH$="(" OR CH$=")") THEN RETURN
    IF (CH$="[" OR CH$="]") THEN RETURN
    IF (CH$="{" OR CH$="}") THEN RETURN
    READ_TOKEN_CONT:
    T$=T$+CH$
    CUR%=CUR%+1
    IF S1 AND S2 THEN S2=0: GOTO READ_TOKEN_LOOP
    IF S1 AND (S2=0) AND (CH$=CHR$(92)) THEN S2=1: GOTO READ_TOKEN_LOOP
    IF S1 AND (S2=0) AND (CH$=CHR$(34)) THEN RETURN
    GOTO READ_TOKEN_LOOP

SKIP_SPACES:
  CH$=MID$(A$,IDX%,1)
  IF (CH$<>" " AND CH$<>",") THEN RETURN
  IDX%=IDX%+1
  GOTO SKIP_SPACES


READ_ATOM:
  R%=0
  RETURN

REM READ_FORM(A$, IDX%) -> R%
READ_FORM:
  IF ER% THEN RETURN
  GOSUB SKIP_SPACES
  GOSUB READ_TOKEN
  REM PRINT "READ_FORM T$: [" + T$ + "]"
  IF (T$="") THEN R%=0: GOTO READ_FORM_DONE
  IF (T$="nil") THEN R%=0: GOTO READ_FORM_DONE
  IF (T$="false") THEN R%=1: GOTO READ_FORM_DONE
  IF (T$="true") THEN R%=2: GOTO READ_FORM_DONE
  CH$=MID$(T$,1,1)
  REM PRINT "CH$: [" + CH$ + "](" + STR$(ASC(CH$)) + ")"
  IF (CH$ >= "0") AND (CH$ <= "9") OR (CH$ = "-") THEN READ_NUMBER
  IF (CH$ = CHR$(34)) THEN READ_STRING
  IF (CH$ = "(") THEN READ_LIST
  IF (CH$ = ")") THEN READ_LIST_END
  GOTO READ_SYMBOL

  READ_NUMBER:
    REM PRINT "READ_NUMBER"
    ZT%(ZI%) = 3
    ZV%(ZI%) = VAL(T$)
    R%=ZI%
    ZI%=ZI%+1
    GOTO READ_FORM_DONE
  READ_STRING:
    REM PRINT "READ_STRING"
    ZT%(ZI%) = 5
    ZV%(ZI%) = ZJ%
    R%=ZI%
    ZI%=ZI%+1
    ZS$(ZJ%) = MID$(T$, 2, LEN(T$)-2)
    REM ZS$(ZJ%) = T$
    ZJ%=ZJ%+1
    GOTO READ_FORM_DONE
  READ_SYMBOL:
    REM PRINT "READ_SYMBOL"
    ZT%(ZI%) = 7
    ZV%(ZI%) = ZJ%
    R%=ZI%
    ZI%=ZI%+1
    ZS$(ZJ%) = T$
    ZJ%=ZJ%+1
    GOTO READ_FORM_DONE

  READ_LIST:
    REM PRINT "READ_LIST"
    REM push start ptr on the stack
    PT%=PT%+1
    PS%(PT%) = ZI%
    REM push current ptr on the stack
    PT%=PT%+1
    PS%(PT%) = ZI%
    GOTO READ_FORM_DONE

  READ_LIST_END:
    REM PRINT "READ_LIST_END"
    IF PT%=-1 THEN ER%=1: ER$="unexpected ')'": RETURN
    REM Set return value to current list
    PT%=PT%-1: REM pop current ptr off the stack
    R%=PS%(PT%): REM start ptr to list
    PT%=PT%-1: REM pop start ptr off the stack
    GOTO READ_FORM_DONE


  READ_FORM_DONE:
    IDX%=IDX%+LEN(T$)
    REM check PS% stack
    IF PT%=-1 THEN RETURN
    IF T$="" THEN ER%=1: ER$="unexpected EOF": RETURN
    REM add list end entry (next pointer is 0 for now)
    REM PRINT "READ_FORM_DONE next list entry"
    ZT%(ZI%) = 8
    ZV%(ZI%) = 0
    REM update prior pointer if not first
    IF PS%(PT%)<>ZI% THEN ZV%(PS%(PT%)) = ZI%
    REM update previous pointer to outself
    PS%(PT%) = ZI%
    ZI%=ZI%+1: REM slot for list element
    GOTO READ_FORM


REM READ_STR(A$) -> R%
READ_STR:
  IDX%=1
  PT%=-1
  GOSUB READ_FORM
  RETURN
