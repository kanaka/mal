REM READ_TOKEN(A$, IDX%) -> T$
READ_TOKEN:
  CUR%=IDX%
  REM PRINT "READ_TOKEN: " + STR$(CUR%) + ", " + MID$(A$,CUR%,1)
  T$=MID$(A$,CUR%,1)
  IF (T$="(") OR (T$=")") THEN RETURN
  IF (T$="[") OR (T$="]") THEN RETURN
  IF (T$="{") OR (T$="}") THEN RETURN
  S1=0: S2=0: REM S1: INSTRING?, S2: ESCAPED?
  IF (T$=CHR$(34)) THEN S1=1
  CUR%=CUR%+1
  READ_TOKEN_LOOP:
    IF CUR% > LEN(A$) THEN RETURN
    CH$=MID$(A$,CUR%,1)
    IF S2 THEN GOTO READ_TOKEN_CONT
    IF S1 THEN GOTO READ_TOKEN_CONT
    IF (CH$=" ") OR (CH$=",") THEN RETURN
    IF (CH$="(") OR (CH$=")") THEN RETURN
    IF (CH$="[") OR (CH$="]") THEN RETURN
    IF (CH$="{") OR (CH$="}") THEN RETURN
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
  IF (T$="nil") THEN T%=0: GOTO READ_SCALAR
  IF (T$="false") THEN T%=1: GOTO READ_SCALAR
  IF (T$="true") THEN T%=2: GOTO READ_SCALAR
  CH$=MID$(T$,1,1)
  REM PRINT "CH$: [" + CH$ + "](" + STR$(ASC(CH$)) + ")"
  IF (CH$ >= "0") AND (CH$ <= "9") THEN READ_NUMBER
  IF (CH$ = "-") THEN READ_SYMBOL_MAYBE

  IF (CH$ = CHR$(34)) THEN READ_STRING
  IF (CH$ = "(") THEN T%=6: GOTO READ_SEQ
  IF (CH$ = ")") THEN T%=6: GOTO READ_SEQ_END
  IF (CH$ = "[") THEN T%=7: GOTO READ_SEQ
  IF (CH$ = "]") THEN T%=7: GOTO READ_SEQ_END
  IF (CH$ = "{") THEN T%=8: GOTO READ_SEQ
  IF (CH$ = "}") THEN T%=8: GOTO READ_SEQ_END
  GOTO READ_SYMBOL

  READ_SCALAR:
    Z%(ZI%,0) = 15
    Z%(ZI%,1) = T%
    R%=ZI%
    ZI%=ZI%+1
    GOTO READ_FORM_DONE
  READ_NUMBER:
    REM PRINT "READ_NUMBER"
    Z%(ZI%,0) = 2
    Z%(ZI%,1) = VAL(T$)
    R%=ZI%
    ZI%=ZI%+1
    GOTO READ_FORM_DONE
  READ_STRING:
    REM PRINT "READ_STRING"
    REM intern string value
    AS$=MID$(T$, 2, LEN(T$)-2): GOSUB STRING
    Z%(ZI%,0) = 4
    Z%(ZI%,1) = R%
    R%=ZI%
    ZI%=ZI%+1
    GOTO READ_FORM_DONE
  READ_SYMBOL_MAYBE:
    CH$=MID$(T$,2,1)
    IF (CH$ >= "0") AND (CH$ <= "9") THEN READ_NUMBER
  READ_SYMBOL:
    REM PRINT "READ_SYMBOL"
    REM intern string value
    AS$=T$: GOSUB STRING
    Z%(ZI%,0) = 5
    Z%(ZI%,1) = R%
    R%=ZI%
    ZI%=ZI%+1
    GOTO READ_FORM_DONE

  READ_SEQ:
    REM PRINT "READ_SEQ"
    SD%=SD%+1: REM increase read sequence depth
    REM push start ptr on the stack
    ZL%=ZL%+1
    ZZ%(ZL%) = ZI%
    REM push current sequence type
    ZL%=ZL%+1
    ZZ%(ZL%) = T%
    REM push current ptr on the stack
    ZL%=ZL%+1
    ZZ%(ZL%) = ZI%
    GOTO READ_FORM_DONE

  READ_SEQ_END:
    REM PRINT "READ_SEQ_END"
    IF SD%=0 THEN ER%=1: ER$="unexpected '" + CH$ + "'": RETURN
    SD%=SD%-1: REM increase read sequence depth
    REM Set return value to current sequence
    ZL%=ZL%-2: REM pop current ptr and type off the stack
    R%=ZZ%(ZL%): REM ptr to start of sequence to return
    ZL%=ZL%-1: REM pop start ptr off the stack
    IF (ZZ%(ZL%+2)) <> T% THEN ER%=1: ER$="sequence mismatch": RETURN
    GOTO READ_FORM_DONE


  READ_FORM_DONE:
    IDX%=IDX%+LEN(T$)
    REM check read sequence depth
    IF SD%=0 THEN RETURN
    IF T$="" THEN ER%=1: ER$="unexpected EOF": RETURN
    REM add list end entry (next pointer is 0 for now)
    REM PRINT "READ_FORM_DONE next list entry"
    Z%(ZI%,0) = ZZ%(ZL%- 1)
    Z%(ZI%,1) = 0
    REM update prior pointer if not first
    IF ZZ%(ZL%)<>ZI% THEN Z%(ZZ%(ZL%),1) = ZI%
    REM update previous pointer to outself
    ZZ%(ZL%) = ZI%
    ZI%=ZI%+1: REM slot for list element
    GOTO READ_FORM


REM READ_STR(A$) -> R%
READ_STR:
  IDX%=1
  SD%=0: REM sequence read depth
  GOSUB READ_FORM
  RETURN
