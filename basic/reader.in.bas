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
    CH$=MID$(A$,RJ,1)
    IF S2 THEN GOTO READ_TOKEN_CONT
    IF S1 THEN GOTO READ_TOKEN_CONT
    IF CH$=" " OR CH$="," THEN RETURN
    IF CH$=" " OR CH$="," OR CH$=CHR$(13) OR CH$=CHR$(10) THEN RETURN
    IF CH$="(" OR CH$=")" OR CH$="[" OR CH$="]" OR CH$="{" OR CH$="}" THEN RETURN
    READ_TOKEN_CONT:
    T$=T$+CH$
    IF T$="~@" THEN RETURN
    RJ=RJ+1
    IF S1 AND S2 THEN S2=0:GOTO READ_TOKEN_LOOP
    IF S1 AND S2=0 AND CH$=CHR$(92) THEN S2=1:GOTO READ_TOKEN_LOOP
    IF S1 AND S2=0 AND CH$=CHR$(34) THEN RETURN
    GOTO READ_TOKEN_LOOP

READ_FILE_CHUNK:
  IF RS=1 THEN RETURN
  IF RI>1 THEN A$=MID$(A$,RI,LEN(A$)-RI+1):RI=1:RJ=RJ-RI+1
  READ_FILE_CHUNK_LOOP:
    IF LEN(A$)>RJ+9 THEN RETURN
    GET#2,C$:A$=A$+C$
    IF (ST AND 64) THEN RS=1:A$=A$+CHR$(10)+")":RETURN
    IF (ST AND 255) THEN RS=1:ER=-1:ER$="File read error "+STR$(ST):RETURN
    GOTO READ_FILE_CHUNK_LOOP

SKIP_SPACES:
  IF RF=1 THEN GOSUB READ_FILE_CHUNK
  CH$=MID$(A$,RI,1)
  IF CH$<>" " AND CH$<>"," AND CH$<>CHR$(13) AND CH$<>CHR$(10) THEN RETURN
  RI=RI+1
  GOTO SKIP_SPACES

SKIP_TO_EOL:
  IF RF=1 THEN GOSUB READ_FILE_CHUNK
  CH$=MID$(A$,RI+1,1)
  RI=RI+1
  IF CH$="" OR CH$=CHR$(13) OR CH$=CHR$(10) THEN RETURN
  GOTO SKIP_TO_EOL


READ_ATOM:
  R=0
  RETURN

REM READ_FORM(A$, RI, RF) -> R
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
  IF T$="^" THEN AS$="with-meta":GOTO READ_MACRO
  IF T$="@" THEN AS$="deref":GOTO READ_MACRO
  CH$=MID$(T$,1,1)
  REM PRINT "CH$: ["+CH$+"]("+STR$(ASC(CH$))+")"
  IF (CH$=";") THEN R=0:GOSUB SKIP_TO_EOL:GOTO READ_FORM
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
    X=X+2:X%(X-1)=(T$="^"):X%(X)=SD: REM push macro type and SD

    REM AS$ is set above
    T=5:GOSUB STRING:X=X+1:X%(X)=R

    SD=0:GOSUB READ_FORM:X=X+1:X%(X)=R

    IF X%(X-3) THEN GOTO READ_MACRO_3

    READ_MACRO_2:
      B2=X%(X-1):B1=X%(X):GOSUB LIST2
      GOTO READ_MACRO_DONE

    READ_MACRO_3:
      SD=0:GOSUB READ_FORM
      B3=X%(X-1):B2=R:B1=X%(X):GOSUB LIST3
      AY=B3:GOSUB RELEASE

    READ_MACRO_DONE:
      REM release values, list has ownership
      AY=B2:GOSUB RELEASE
      AY=B1:GOSUB RELEASE

      SD=X%(X-2):X=X-4: REM get SD and pop the stack
      T$="": REM necessary to prevent unexpected EOF errors
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

    REM point to empty sequence to start off
    R=(T-5)*2+1: REM calculate location of empty seq
    Z%(R,0)=Z%(R,0)+32

    REM push start ptr on the stack
    X=X+1
    X%(X)=R
    REM push current sequence type
    X=X+1
    X%(X)=T
    REM push previous ptr on the stack
    X=X+1
    X%(X)=R

    RI=RI+LEN(T$)
    GOTO READ_FORM

  READ_SEQ_END:
    REM PRINT "READ_SEQ_END"
    IF SD=0 THEN ER$="unexpected '"+CH$+"'":GOTO READ_FORM_ABORT
    IF X%(X-1)<>T THEN ER$="sequence mismatch":GOTO READ_FORM_ABORT
    SD=SD-1: REM decrease read sequence depth
    R=X%(X-2): REM ptr to start of sequence to return
    T=X%(X-1): REM type prior to recur
    X=X-3: REM pop start, type and previous off the stack
    GOTO READ_FORM_DONE


  READ_FORM_DONE:
    RI=RI+LEN(T$)

    REM check read sequence depth
    IF SD=0 THEN RETURN

    REM previous element
    T7=X%(X)

    REM allocate new sequence entry, set type to previous type, set
    REM next to previous next or previous (if first)
    L=Z%(T7,1)
    IF T7<9 THEN L=T7
    T8=R: REM save previous value for release
    T=X%(X-1):N=R:GOSUB ALLOC
    AY=T8:GOSUB RELEASE: REM list takes ownership

    REM if previous element is the first element then set
    REM the first to the new element
    IF T7<9 THEN X%(X-2)=R:GOTO READ_FORM_SKIP_FIRST
    REM set previous list element to point to new element
    Z%(T7,1)=R

    READ_FORM_SKIP_FIRST:

    REM update previous pointer to current element
    X%(X)=R
    GOTO READ_FORM

  READ_FORM_ABORT:
    ER=-1
    R=0
    READ_FORM_ABORT_UNWIND:
      IF SD=0 THEN RETURN
      X=X-3: REM pop previous, type, and start off the stack
      SD=SD-1
      IF SD=0 THEN AY=X%(X+1):GOSUB RELEASE
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
  RS=0: REM file read state (1: EOF)
  SD=0: REM sequence read depth
  OPEN 2,8,0,A$
  REM READ_FILE_CHUNK adds terminating ")"
  A$="(do ":GOSUB READ_FORM
  CLOSE 2
  RETURN
