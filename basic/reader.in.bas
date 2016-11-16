REM READ_TOKEN(A$, RI, RF) -> T$
READ_TOKEN:
  GOSUB SKIP_SPACES
  RJ=RI
  IF RF=1 THEN GOSUB READ_FILE_CHUNK
  REM PRINT "READ_TOKEN: "+STR$(RJ)+", "+MID$(A$,RJ,1)
  T$=MID$(A$,RJ,1)
  IF T$=";" THEN GOSUB SKIP_TO_EOL:GOTO READ_TOKEN
  IF T$="(" OR T$=")" OR T$="[" OR T$="]" OR T$="{" OR T$="}" THEN RETURN
  IF T$="'" OR T$="`" OR T$="@" THEN RETURN
  IF T$="~" AND NOT MID$(A$,RJ+1,1)="@" THEN RETURN
  S1=0:S2=0: REM S1: INSTRING?, S2: ESCAPED?
  IF T$=CHR$(34) THEN S1=1
  RJ=RJ+1
  READ_TOKEN_LOOP:
    IF RF=1 THEN GOSUB READ_FILE_CHUNK
    IF RJ>LEN(A$) THEN RETURN
    C$=MID$(A$,RJ,1)
    IF S2 THEN GOTO READ_TOKEN_CONT
    IF S1 THEN GOTO READ_TOKEN_CONT
    IF C$=" " OR C$="," THEN RETURN
    IF C$=" " OR C$="," OR C$=CHR$(13) OR C$=CHR$(10) THEN RETURN
    IF C$="(" OR C$=")" OR C$="[" OR C$="]" OR C$="{" OR C$="}" THEN RETURN
    READ_TOKEN_CONT:
    T$=T$+C$
    IF T$="~@" THEN RETURN
    RJ=RJ+1
    IF S1 AND S2 THEN S2=0:GOTO READ_TOKEN_LOOP
    IF S1 AND S2=0 AND C$=CHR$(92) THEN S2=1:GOTO READ_TOKEN_LOOP
    IF S1 AND S2=0 AND C$=CHR$(34) THEN RETURN
    GOTO READ_TOKEN_LOOP

READ_FILE_CHUNK:
  IF EZ=1 THEN RETURN
  IF RI>1 THEN A$=MID$(A$,RI,LEN(A$)-RI+1):RI=1:RJ=RJ-RI+1
  READ_FILE_CHUNK_LOOP:
    IF LEN(A$)>RJ+9 THEN RETURN
    #cbm GET#2,C$
    #qbasic C$=INPUT$(1,2)
    #qbasic IF EOF(2) THEN EZ=1:A$=A$+CHR$(10)+")":RETURN
    A$=A$+C$
    #cbm IF (ST AND 64) THEN EZ=1:A$=A$+CHR$(10)+")":RETURN
    #cbm IF (ST AND 255) THEN EZ=1:ER=-1:E$="File read error "+STR$(ST):RETURN
    GOTO READ_FILE_CHUNK_LOOP

SKIP_SPACES:
  IF RF=1 THEN GOSUB READ_FILE_CHUNK
  C$=MID$(A$,RI,1)
  IF C$<>" " AND C$<>"," AND C$<>CHR$(13) AND C$<>CHR$(10) THEN RETURN
  RI=RI+1
  GOTO SKIP_SPACES

SKIP_TO_EOL:
  IF RF=1 THEN GOSUB READ_FILE_CHUNK
  C$=MID$(A$,RI+1,1)
  RI=RI+1
  IF C$="" OR C$=CHR$(13) OR C$=CHR$(10) THEN RETURN
  GOTO SKIP_TO_EOL


REM READ_FORM(A$, RI, RF) -> R
SUB READ_FORM
  Q=T:GOSUB PUSH_Q: REM save current value of T
  READ_FORM_RECUR:
  IF ER<>-2 THEN GOTO READ_FORM_RETURN
  GOSUB READ_TOKEN
  REM PRINT "READ_FORM T$: ["+T$+"]"
  IF T$="" THEN R=0:Z%(R,0)=Z%(R,0)+32:GOTO READ_FORM_RETURN
  IF T$="nil" THEN T=0:GOTO READ_NIL_BOOL
  IF T$="false" THEN T=1:GOTO READ_NIL_BOOL
  IF T$="true" THEN T=2:GOTO READ_NIL_BOOL
  IF T$="'" THEN B$="quote":GOTO READ_MACRO
  IF T$="`" THEN B$="quasiquote":GOTO READ_MACRO
  IF T$="~" THEN B$="unquote":GOTO READ_MACRO
  IF T$="~@" THEN B$="splice-unquote":GOTO READ_MACRO
  IF T$="^" THEN B$="with-meta":GOTO READ_MACRO
  IF T$="@" THEN B$="deref":GOTO READ_MACRO
  C$=MID$(T$,1,1)
  REM PRINT "C$: ["+C$+"]("+STR$(ASC(C$))+")"
  IF C$>="0" AND C$<="9" THEN GOTO READ_NUMBER
  IF C$="-" THEN GOTO READ_SYMBOL_MAYBE

  IF C$=CHR$(34) THEN GOTO READ_STRING
  IF C$=":" THEN GOTO READ_KEYWORD
  REM set end character in Q and read the sequence
  IF C$="(" THEN T=6:Q=ASC(")"):GOTO READ_SEQ_START
  IF C$="[" THEN T=7:Q=ASC("]"):GOTO READ_SEQ_START
  IF C$="{" THEN T=8:Q=ASC("}"):GOTO READ_SEQ_START
  IF C$=")" OR C$="]" OR C$="}" THEN R=-1:ER=-1:E$="unexpected "+C$:GOTO READ_FORM_RETURN
  GOTO READ_SYMBOL

  READ_NIL_BOOL:
    REM PRINT "READ_NIL_BOOL"
    R=T
    Z%(R,0)=Z%(R,0)+32
    GOTO READ_FORM_RETURN
  READ_NUMBER:
    REM PRINT "READ_NUMBER"
    T=2:L=VAL(T$):GOSUB ALLOC
    GOTO READ_FORM_RETURN
  READ_MACRO:
    RI=RI+LEN(T$)
    REM push macro type
    Q=-1*(T$="^"):GOSUB PUSH_Q

    REM B$ is set above
    T=5:GOSUB STRING
    REM push string
    GOSUB PUSH_R

    CALL READ_FORM
    REM push first form
    GOSUB PUSH_R
    IF ER>-2 THEN GOTO READ_MACRO_DONE

    GOSUB PEEK_Q_2
    IF Q THEN GOTO READ_MACRO_3

    READ_MACRO_2:
      GOSUB PEEK_Q_1:B=Q
      GOSUB PEEK_Q:A=Q
      GOSUB LIST2
      GOTO READ_MACRO_DONE

    READ_MACRO_3:
      CALL READ_FORM
      GOSUB PEEK_Q_1:C=Q
      B=R
      GOSUB PEEK_Q:A=Q
      GOSUB LIST3
      AY=C:GOSUB RELEASE

    READ_MACRO_DONE:
      REM release values, list has ownership
      AY=B:GOSUB RELEASE
      AY=A:GOSUB RELEASE

      REM pop the stack
      GOSUB POP_Q: REM pop first form
      GOSUB POP_Q: REM pop string
      GOSUB POP_Q: REM pop macro type
      T$="": REM necessary to prevent unexpected EOF errors
      GOTO READ_FORM_RETURN

  READ_STRING:
    REM PRINT "READ_STRING"
    C=ASC(MID$(T$,LEN(T$),1))
    IF C<>34 THEN R=-1:ER=-1:E$="expected '"+CHR$(34)+"'":GOTO READ_FORM_RETURN
    R$=MID$(T$,2,LEN(T$)-2)
    S1$=CHR$(92)+CHR$(34):S2$=CHR$(34):GOSUB REPLACE: REM unescape quotes
    S1$=CHR$(92)+"n":S2$=CHR$(13):GOSUB REPLACE: REM unescape newlines
    S1$=CHR$(92)+CHR$(92):S2$=CHR$(92):GOSUB REPLACE: REM unescape backslashes
    REM intern string value
    B$=R$:T=4:GOSUB STRING
    GOTO READ_FORM_RETURN
  READ_KEYWORD:
    R$=CHR$(127)+MID$(T$,2,LEN(T$)-1)
    B$=R$:T=4:GOSUB STRING
    GOTO READ_FORM_RETURN
  READ_SYMBOL_MAYBE:
    C$=MID$(T$,2,1)
    IF C$>="0" AND C$<="9" THEN GOTO READ_NUMBER
  READ_SYMBOL:
    REM PRINT "READ_SYMBOL"
    B$=T$:T=5:GOSUB STRING
    GOTO READ_FORM_RETURN

  READ_SEQ_START:
    RI=RI+LEN(T$)
    SD=SD+1

    GOSUB PUSH_Q: REM push return character

    REM setup the stack for the loop
    GOSUB MAP_LOOP_START

  READ_SEQ_LOOP:
    GOSUB READ_TOKEN: REM peek at token
    IF T$="" THEN ER=-1:E$="unexpected EOF"
    Q=3:GOSUB PEEK_Q_Q
    IF ER<>-2 OR T$=CHR$(Q) THEN GOTO READ_SEQ_DONE

    CALL READ_FORM

    REM if error, release the unattached element
    IF ER<>-2 THEN AY=R:GOSUB RELEASE:GOTO READ_SEQ_DONE

    REM if this is a hash-map, READ_FORM again
    IF T=8 THEN GOSUB PUSH_R:CALL READ_FORM
    IF T=8 THEN GOSUB POP_Q:M=Q: REM key value

    REM main value
    REM for list/vector this is result of the first READ_FORM 
    N=R


    REM update the return sequence structure
    REM release N since list takes full ownership
    C=1:GOSUB MAP_LOOP_UPDATE

    GOTO READ_SEQ_LOOP

  READ_SEQ_DONE:
    SD=SD-1
    REM cleanup stack and get return value
    GOSUB MAP_LOOP_DONE

    GOSUB POP_Q: REM pop end character ptr
REM P1=R:PRINT "READ_SEQ R:":GOSUB PR_OBJECT
    GOTO READ_FORM_RETURN

  READ_FORM_RETURN:
REM    IF ER<>-2 THEN R=0:Z%(R,0)=Z%(R,0)+32
    RI=RI+LEN(T$)
    GOSUB POP_Q:T=Q: REM restore current value of T

END SUB


REM READ_STR(A$) -> R
READ_STR:
  RI=1: REM index into A$
  RF=0: REM not reading from file
  SD=0: REM sequence read depth
  CALL READ_FORM
  RETURN

REM READ_FILE(A$) -> R
READ_FILE:
  RI=1: REM index into A$
  RJ=1: REM READ_TOKEN sub-index
  RF=1: REM reading from file
  EZ=0: REM file read state (1: EOF)
  SD=0: REM sequence read depth
  #cbm OPEN 2,8,0,A$
  #qbasic IF NOT _FILEEXISTS(A$) THEN ER=-1:E$="File not found":RETURN
  #qbasic OPEN A$ FOR INPUT AS #2
  REM READ_FILE_CHUNK adds terminating ")"
  A$="(do "
  CALL READ_FORM
  CLOSE 2
  EZ=0
  RETURN
