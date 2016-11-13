REM READ_TOKEN(A$, RI, RF) -> T$
READ_TOKEN:
  RJ=RI
  IF RF=1 THEN GOSUB READ_FILE_CHUNK
  REM PRINT "READ_TOKEN: "+STR$(RJ)+", "+MID$(A$,RJ,1)
  T$=MID$(A$,RJ,1)
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


READ_ATOM:
  R=0
  RETURN

REM READ_FORM(A$, RI, RF) -> R
READ_FORM:
  IF ER<>-2 THEN RETURN
  GOSUB SKIP_SPACES
  GOSUB READ_TOKEN
  IF T$="" AND SD>0 THEN E$="unexpected EOF":GOTO READ_FORM_ABORT
  REM PRINT "READ_FORM T$: ["+T$+"]"
  IF T$="" THEN R=0:GOTO READ_FORM_DONE
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
  IF (C$=";") THEN R=0:GOSUB SKIP_TO_EOL:GOTO READ_FORM
  IF C$>="0" AND C$<="9" THEN GOTO READ_NUMBER
  IF C$="-" THEN GOTO READ_SYMBOL_MAYBE

  IF C$=CHR$(34) THEN GOTO READ_STRING
  IF C$=":" THEN GOTO READ_KEYWORD
  IF C$="(" THEN T=6:GOTO READ_SEQ
  IF C$=")" THEN T=6:GOTO READ_SEQ_END
  IF C$="[" THEN T=7:GOTO READ_SEQ
  IF C$="]" THEN T=7:GOTO READ_SEQ_END
  IF C$="{" THEN T=8:GOTO READ_SEQ
  IF C$="}" THEN T=8:GOTO READ_SEQ_END
  GOTO READ_SYMBOL

  READ_NIL_BOOL:
    REM PRINT "READ_NIL_BOOL"
    R=T
    Z%(R,0)=Z%(R,0)+32
    GOTO READ_FORM_DONE
  READ_NUMBER:
    REM PRINT "READ_NUMBER"
    T=2:L=VAL(T$):GOSUB ALLOC
    GOTO READ_FORM_DONE
  READ_MACRO:
    RI=RI+LEN(T$)
    REM to call READ_FORM recursively, SD needs to be saved, set to
    REM 0 for the call and then restored afterwards.
    REM push macro type and SD
    Q=-1*(T$="^"):GOSUB PUSH_Q
    Q=SD:GOSUB PUSH_Q

    REM B$ is set above
    T=5:GOSUB STRING
    GOSUB PUSH_R

    SD=0:GOSUB READ_FORM
    GOSUB PUSH_R

    Q=3:GOSUB PEEK_Q_Q
    IF Q THEN GOTO READ_MACRO_3

    READ_MACRO_2:
      GOSUB PEEK_Q_1:B=Q
      GOSUB PEEK_Q:A=Q
      GOSUB LIST2
      GOTO READ_MACRO_DONE

    READ_MACRO_3:
      SD=0:GOSUB READ_FORM
      GOSUB PEEK_Q_1:C=Q
      B=R
      GOSUB PEEK_Q:A=Q
      GOSUB LIST3
      AY=C:GOSUB RELEASE

    READ_MACRO_DONE:
      REM release values, list has ownership
      AY=B:GOSUB RELEASE
      AY=A:GOSUB RELEASE

      REM get SD and pop the stack
      GOSUB POP_Q
      GOSUB POP_Q
      GOSUB POP_Q:SD=Q
      GOSUB POP_Q
      T$="": REM necessary to prevent unexpected EOF errors
      GOTO READ_FORM_DONE
  READ_STRING:
    REM PRINT "READ_STRING"
    C=ASC(MID$(T$,LEN(T$),1))
    IF C<>34 THEN E$="expected '"+CHR$(34)+"'":GOTO READ_FORM_ABORT
    R$=MID$(T$,2,LEN(T$)-2)
    S1$=CHR$(92)+CHR$(34):S2$=CHR$(34):GOSUB REPLACE: REM unescape quotes
    S1$=CHR$(92)+"n":S2$=CHR$(13):GOSUB REPLACE: REM unescape newlines
    S1$=CHR$(92)+CHR$(92):S2$=CHR$(92):GOSUB REPLACE: REM unescape backslashes
    REM intern string value
    B$=R$:T=4:GOSUB STRING
    GOTO READ_FORM_DONE
  READ_KEYWORD:
    R$=CHR$(127)+MID$(T$,2,LEN(T$)-1)
    B$=R$:T=4:GOSUB STRING
    GOTO READ_FORM_DONE
  READ_SYMBOL_MAYBE:
    C$=MID$(T$,2,1)
    IF C$>="0" AND C$<="9" THEN GOTO READ_NUMBER
  READ_SYMBOL:
    REM PRINT "READ_SYMBOL"
    B$=T$:T=5:GOSUB STRING
    GOTO READ_FORM_DONE

  READ_SEQ:
    REM PRINT "READ_SEQ"
    SD=SD+1: REM increase read sequence depth

    REM point to empty sequence to start off
    R=(T-5)*2+1: REM calculate location of empty seq
    Z%(R,0)=Z%(R,0)+32

    REM push start ptr on the stack
    GOSUB PUSH_R
    REM push current sequence type
    Q=T:GOSUB PUSH_Q
    REM push previous ptr on the stack
    GOSUB PUSH_R

    RI=RI+LEN(T$)
    GOTO READ_FORM

  READ_SEQ_END:
    REM PRINT "READ_SEQ_END"
    IF SD=0 THEN E$="unexpected '"+C$+"'":GOTO READ_FORM_ABORT
    GOSUB PEEK_Q_1
    IF Q<>T THEN E$="sequence mismatch":GOTO READ_FORM_ABORT
    SD=SD-1: REM decrease read sequence depth
    GOSUB POP_Q: REM pop previous
    GOSUB POP_Q:T=Q: REM type prior to recur
    GOSUB POP_R: REM ptr to start of sequence to return
    GOTO READ_FORM_DONE


  READ_FORM_DONE:
    RI=RI+LEN(T$)

    REM check read sequence depth
    IF SD=0 THEN RETURN

    GOSUB PEEK_Q: REM previous element

    REM allocate new sequence entry, set type to previous type, set
    REM next to previous next or previous (if first)
    L=Z%(Q,1)
    IF Q<9 THEN L=Q
    AY=R: REM save previous value for release
    GOSUB PEEK_Q_1:T=Q
    N=R:GOSUB ALLOC
    REM list takes ownership
    GOSUB RELEASE
    IF L<9 THEN AY=L:GOSUB RELEASE

    REM if previous element is the first element then set
    REM the first to the new element
    GOSUB PEEK_Q: REM previous element
    IF Q<9 THEN Q=R:GOSUB PUT_Q_2:GOTO READ_FORM_SKIP_FIRST
    REM set previous list element to point to new element
    Z%(Q,1)=R

    READ_FORM_SKIP_FIRST:

    REM update previous pointer to current element
    Q=R:GOSUB PUT_Q
    GOTO READ_FORM

  READ_FORM_ABORT:
    ER=-1
    R=0
    READ_FORM_ABORT_UNWIND:
      IF SD=0 THEN RETURN
      SD=SD-1: REM decrease read sequence depth
      REM pop previous, type, and start off the stack
      GOSUB POP_Q
      GOSUB POP_Q
      GOSUB POP_Q:AY=Q
      IF SD=0 THEN GOSUB RELEASE
      GOTO READ_FORM_ABORT_UNWIND


REM READ_STR(A$) -> R
READ_STR:
  RI=1: REM index into A$
  RF=0: REM not reading from file
  SD=0: REM sequence read depth
  GOSUB READ_FORM
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
  A$="(do ":GOSUB READ_FORM
  CLOSE 2
  EZ=0
  RETURN
