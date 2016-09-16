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
  IF (T$="nil") THEN T%=0: GOTO READ_NIL_BOOL
  IF (T$="false") THEN T%=1: GOTO READ_NIL_BOOL
  IF (T$="true") THEN T%=2: GOTO READ_NIL_BOOL
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

  READ_NIL_BOOL:
    REM PRINT "READ_NIL_BOOL"
    SZ%=1: GOSUB ALLOC
    Z%(R%,0) = 14+16
    Z%(R%,1) = T%
    GOTO READ_FORM_DONE
  READ_NUMBER:
    REM PRINT "READ_NUMBER"
    SZ%=1: GOSUB ALLOC
    Z%(R%,0) = 2+16
    Z%(R%,1) = VAL(T$)
    GOTO READ_FORM_DONE
  READ_STRING:
    T7$=MID$(T$,LEN(T$),1)
    IF T7$<>CHR$(34) THEN ER$="expected '"+CHR$(34)+"'": GOTO READ_FORM_ABORT
    REM PRINT "READ_STRING"
    REM intern string value
    AS$=MID$(T$, 2, LEN(T$)-2): GOSUB STRING
    T7%=R%
    SZ%=1: GOSUB ALLOC
    Z%(R%,0) = 4+16
    Z%(R%,1) = T7%
    GOTO READ_FORM_DONE
  READ_SYMBOL_MAYBE:
    CH$=MID$(T$,2,1)
    IF (CH$ >= "0") AND (CH$ <= "9") THEN READ_NUMBER
  READ_SYMBOL:
    REM PRINT "READ_SYMBOL"
    REM intern string value
    AS$=T$: GOSUB STRING
    T7%=R%
    SZ%=1: GOSUB ALLOC
    Z%(R%,0) = 5+16
    Z%(R%,1) = T7%
    GOTO READ_FORM_DONE

  READ_SEQ:
    REM PRINT "READ_SEQ"
    SD%=SD%+1: REM increase read sequence depth

    REM allocate first sequence entry and space for value
    SZ%=2: GOSUB ALLOC

    REM set reference value/pointer to new embedded sequence
    IF SD%>1 THEN Z%(ZZ%(ZL%)+1,1)=R%

    REM set the type (with 1 ref cnt) and next pointer to current end
    Z%(R%,0) = T%+16
    Z%(R%,1) = 0
    Z%(R%+1,0) = 14
    Z%(R%+1,1) = 0

    REM push start ptr on the stack
    ZL%=ZL%+1
    ZZ%(ZL%) = R%
    REM push current sequence type
    ZL%=ZL%+1
    ZZ%(ZL%) = T%
    REM push previous ptr on the stack
    ZL%=ZL%+1
    ZZ%(ZL%) = R%

    IDX%=IDX%+LEN(T$)
    GOTO READ_FORM

  READ_SEQ_END:
    REM PRINT "READ_SEQ_END"
    IF SD%=0 THEN ER$="unexpected '" + CH$ + "'": GOTO READ_FORM_ABORT
    IF ZZ%(ZL%-1)<>T% THEN ER$="sequence mismatch": GOTO READ_FORM_ABORT
    SD%=SD%-1: REM decrease read sequence depth
    R%=ZZ%(ZL%-2): REM ptr to start of sequence to return
    T%=ZZ%(ZL%-1): REM type prior to recur
    ZL%=ZL%-3: REM pop previous, type, and start off the stack
    GOTO READ_FORM_DONE


  READ_FORM_DONE:
    IDX%=IDX%+LEN(T$)

    T8%=R%: REM save previous value

    REM check read sequence depth
    IF SD%=0 THEN RETURN
    IF T$="" THEN ER$="unexpected EOF": GOTO READ_FORM_ABORT
    REM PRINT "READ_FORM_DONE next list entry"

    REM allocate new sequence entry and space for value
    SZ%=2: GOSUB ALLOC

    REM previous element
    T7%=ZZ%(ZL%)
    REM set previous list element to point to new element
    Z%(T7%,1) = R%
    REM set the list value pointer
    Z%(T7%+1,1)=T8%
    REM set type to previous type, with ref count of 1 (from previous)
    Z%(R%,0) = ZZ%(ZL%-1)+16
    Z%(R%,1) = 0: REM current end of sequence
    Z%(R%+1,0) = 14
    Z%(R%+1,1) = 0

    IF T7%=ZZ%(ZL%-2) THEN GOTO READ_FORM_SKIP_FIRST
    Z%(T7%,1) = R%

    READ_FORM_SKIP_FIRST:
    REM update previous pointer to current element
    ZZ%(ZL%) = R%
    GOTO READ_FORM

  READ_FORM_ABORT:
    ER%=1
    R%=0
    READ_FORM_ABORT_UNWIND:
      IF SD%=0 THEN RETURN
      ZL%=ZL%-3: REM pop previous, type, and start off the stack
      SD%=SD%-1
      IF SD%=0 THEN AY%=ZZ%(ZL%+1): GOSUB RELEASE
      GOTO READ_FORM_ABORT_UNWIND



REM READ_STR(A$) -> R%
READ_STR:
  IDX%=1
  SD%=0: REM sequence read depth
  GOSUB READ_FORM
  RETURN
