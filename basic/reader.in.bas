REM READ_TOKEN(A$, IDX%) -> T$
READ_TOKEN:
  CUR%=IDX%
  REM PRINT "READ_TOKEN: "+STR$(CUR%)+", "+MID$(A$,CUR%,1)
  T$=MID$(A$,CUR%,1)
  IF T$="(" OR T$=")" OR T$="[" OR T$="]" OR T$="{" OR T$="}" THEN RETURN
  IF T$="'" OR T$="`" OR T$="@" THEN RETURN
  IF T$="~" AND NOT MID$(A$,CUR%+1,1)="@" THEN RETURN
  S1=0:S2=0: REM S1: INSTRING?, S2: ESCAPED?
  IF T$=CHR$(34) THEN S1=1
  CUR%=CUR%+1
  READ_TOKEN_LOOP:
    IF CUR%>LEN(A$) THEN RETURN
    CH$=MID$(A$,CUR%,1)
    IF S2 THEN GOTO READ_TOKEN_CONT
    IF S1 THEN GOTO READ_TOKEN_CONT
    IF CH$=" " OR CH$="," THEN RETURN
    IF CH$="(" OR CH$=")" OR CH$="[" OR CH$="]" OR CH$="{" OR CH$="}" THEN RETURN
    READ_TOKEN_CONT:
    T$=T$+CH$
    IF T$="~@" THEN RETURN
    CUR%=CUR%+1
    IF S1 AND S2 THEN S2=0:GOTO READ_TOKEN_LOOP
    IF S1 AND S2=0 AND CH$=CHR$(92) THEN S2=1:GOTO READ_TOKEN_LOOP
    IF S1 AND S2=0 AND CH$=CHR$(34) THEN RETURN
    GOTO READ_TOKEN_LOOP

SKIP_SPACES:
  CH$=MID$(A$,IDX%,1)
  IF (CH$<>" ") AND (CH$<>",") AND (CH$<>CHR$(13)) AND (CH$<>CHR$(10)) THEN RETURN
  IDX%=IDX%+1
  GOTO SKIP_SPACES


READ_ATOM:
  R=0
  RETURN

REM READ_FORM(A$, IDX%) -> R
READ_FORM:
  IF ER<>-2 THEN RETURN
  GOSUB SKIP_SPACES
  GOSUB READ_TOKEN
  IF T$="" AND SD>0 THEN ER$="unexpected EOF":GOTO READ_FORM_ABORT
  REM PRINT "READ_FORM T$: ["+T$+"]"
  IF T$="" THEN R=0:GOTO READ_FORM_DONE
  IF T$="nil" THEN T=0:GOTO READ_NIL_BOOL
  IF T$="false" THEN T=1:GOTO READ_NIL_BOOL
  IF T$="true" THEN T=2:GOTO READ_NIL_BOOL
  IF T$="'" THEN AS$="quote":GOTO READ_MACRO
  IF T$="`" THEN AS$="quasiquote":GOTO READ_MACRO
  IF T$="~" THEN AS$="unquote":GOTO READ_MACRO
  IF T$="~@" THEN AS$="splice-unquote":GOTO READ_MACRO
  IF T$="@" THEN AS$="deref":GOTO READ_MACRO
  CH$=MID$(T$,1,1)
  REM PRINT "CH$: ["+CH$+"]("+STR$(ASC(CH$))+")"
  IF (CH$=";") THEN R=0:GOTO READ_TO_EOL
  IF CH$>="0" AND CH$<="9" THEN GOTO READ_NUMBER
  IF CH$="-" THEN GOTO READ_SYMBOL_MAYBE

  IF CH$=CHR$(34) THEN GOTO READ_STRING
  IF CH$=":" THEN GOTO READ_KEYWORD
  IF CH$="(" THEN T=6:GOTO READ_SEQ
  IF CH$=")" THEN T=6:GOTO READ_SEQ_END
  IF CH$="[" THEN T=7:GOTO READ_SEQ
  IF CH$="]" THEN T=7:GOTO READ_SEQ_END
  IF CH$="{" THEN T=8:GOTO READ_SEQ
  IF CH$="}" THEN T=8:GOTO READ_SEQ_END
  GOTO READ_SYMBOL

  READ_TO_EOL:
    CH$=MID$(A$,IDX%+1,1)
    IDX%=IDX%+1
    IF CH$="" OR CH$=CHR$(13) OR CH$=CHR$(10) THEN GOTO READ_FORM
    GOTO READ_TO_EOL
  READ_NIL_BOOL:
    REM PRINT "READ_NIL_BOOL"
    R=T
    Z%(R,0)=Z%(R,0)+16
    GOTO READ_FORM_DONE
  READ_NUMBER:
    REM PRINT "READ_NUMBER"
    T=2:L=VAL(T$):GOSUB ALLOC
    GOTO READ_FORM_DONE
  READ_MACRO:
    IDX%=IDX%+LEN(T$)
    T=5:GOSUB STRING: REM AS$ set above

    REM to call READ_FORM recursively, SD needs to be saved, set to
    REM 0 for the call and then restored afterwards.
    X=X+2:S%(X-1)=SD:S%(X)=R: REM push SD and symbol
    SD=0:GOSUB READ_FORM:B1%=R
    SD=S%(X-1):B2%=S%(X):X=X-2: REM pop SD, pop symbol into B2%

    GOSUB LIST2
    REM release values, list has ownership
    AY=B1%:GOSUB RELEASE
    AY=B2%:GOSUB RELEASE

    T$=""
    GOTO READ_FORM_DONE
  READ_STRING:
    REM PRINT "READ_STRING"
    T7$=MID$(T$,LEN(T$),1)
    IF T7$<>CHR$(34) THEN ER$="expected '"+CHR$(34)+"'":GOTO READ_FORM_ABORT
    R$=MID$(T$,2,LEN(T$)-2)
    S1$=CHR$(92)+CHR$(34):S2$=CHR$(34):GOSUB REPLACE: REM unescape quotes
    S1$=CHR$(92)+"n":S2$=CHR$(13):GOSUB REPLACE: REM unescape newlines
    S1$=CHR$(92)+CHR$(92):S2$=CHR$(92):GOSUB REPLACE: REM unescape backslashes
    REM intern string value
    AS$=R$:T=4:GOSUB STRING
    GOTO READ_FORM_DONE
  READ_KEYWORD:
    R$=CHR$(127)+MID$(T$,2,LEN(T$)-1)
    AS$=R$:T=4:GOSUB STRING
    GOTO READ_FORM_DONE
  READ_SYMBOL_MAYBE:
    CH$=MID$(T$,2,1)
    IF CH$>="0" AND CH$<="9" THEN GOTO READ_NUMBER
  READ_SYMBOL:
    REM PRINT "READ_SYMBOL"
    AS$=T$:T=5:GOSUB STRING
    GOTO READ_FORM_DONE

  READ_SEQ:
    REM PRINT "READ_SEQ"
    SD=SD+1: REM increase read sequence depth

    REM allocate first sequence entry and space for value
    L=0:N=0:GOSUB ALLOC: REM T alread set above

    REM set reference value/pointer to new embedded sequence
    IF SD>1 THEN Z%(S%(X)+1,1)=R

    REM push start ptr on the stack
    X=X+1
    S%(X)=R
    REM push current sequence type
    X=X+1
    S%(X)=T
    REM push previous ptr on the stack
    X=X+1
    S%(X)=R

    IDX%=IDX%+LEN(T$)
    GOTO READ_FORM

  READ_SEQ_END:
    REM PRINT "READ_SEQ_END"
    IF SD=0 THEN ER$="unexpected '"+CH$+"'":GOTO READ_FORM_ABORT
    IF S%(X-1)<>T THEN ER$="sequence mismatch":GOTO READ_FORM_ABORT
    SD=SD-1: REM decrease read sequence depth
    R=S%(X-2): REM ptr to start of sequence to return
    T=S%(X-1): REM type prior to recur
    X=X-3: REM pop previous, type, and start off the stack
    GOTO READ_FORM_DONE


  READ_FORM_DONE:
    IDX%=IDX%+LEN(T$)

    T8=R: REM save previous value

    REM check read sequence depth
    IF SD=0 THEN RETURN
    REM PRINT "READ_FORM_DONE next list entry"

    REM allocate new sequence entry and space for value
    REM set type to previous type, with ref count of 1 (from previous)
    T=S%(X-1):L=0:N=0:GOSUB ALLOC

    REM previous element
    T7=S%(X)
    REM set previous list element to point to new element
    Z%(T7,1)=R
    REM set the list value pointer
    Z%(T7+1,1)=T8

    IF T7=S%(X-2) THEN GOTO READ_FORM_SKIP_FIRST
    Z%(T7,1)=R

    READ_FORM_SKIP_FIRST:
    REM update previous pointer to current element
    S%(X)=R
    GOTO READ_FORM

  READ_FORM_ABORT:
    ER=-1
    R=0
    READ_FORM_ABORT_UNWIND:
      IF SD=0 THEN RETURN
      X=X-3: REM pop previous, type, and start off the stack
      SD=SD-1
      IF SD=0 THEN AY=S%(X+1):GOSUB RELEASE
      GOTO READ_FORM_ABORT_UNWIND


REM READ_STR(A$) -> R
READ_STR:
  IDX%=1
  SD=0: REM sequence read depth
  GOSUB READ_FORM
  RETURN
