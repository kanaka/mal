REM READ_TOKEN(RF=0, A$, RI) -> T$
REM READ_TOKEN(RF=1) -> T$
READ_TOKEN:
  IF RF=1 THEN RF=2:T$="(":RETURN
  IF RF=2 THEN RF=3:T$="do":RETURN
  GOSUB SKIP_SPACES
  REM PRINT "READ_TOKEN: "+STR$(RI)+", "+MID$(A$,RI,1)
  GOSUB READ_CHAR
  IF C$=";" THEN GOSUB SKIP_TO_EOL:GOTO READ_TOKEN
  T$=C$
  IF T$="(" OR T$=")" OR T$="[" OR T$="]" OR T$="{" OR T$="}" OR T$="'" OR T$="`" OR T$="@" THEN RETURN
  GOSUB PEEK_CHAR: REM peek at next character
  IF T$="~" AND C$<>"@" THEN RETURN
  S1=0:S2=0: REM S1: INSTRING?, S2: ESCAPED?
  IF T$=CHR$(34) THEN S1=1
  READ_TOKEN_LOOP:
    GOSUB PEEK_CHAR: REM peek at next character
    IF C$="" THEN RETURN
    IF S1 THEN GOTO READ_TOKEN_CONT
    IF C$=" " OR C$="," OR C$=CHR$(13) OR C$=CHR$(10) THEN RETURN
    IF C$="(" OR C$=")" OR C$="[" OR C$="]" OR C$="{" OR C$="}" THEN RETURN
    READ_TOKEN_CONT:
    GOSUB READ_CHAR
    T$=T$+C$
    IF T$="~@" THEN RETURN
    IF S1=0 OR S2=1 THEN S2=0:GOTO READ_TOKEN_LOOP
    REM S1=1 (INSTRING?) and S2=0 (not ESCAPED?)
    IF C$=CHR$(92) THEN S2=1
    IF C$=CHR$(34) THEN RETURN
    GOTO READ_TOKEN_LOOP


REM READ_CHAR(A$, RI) -> C$
READ_CHAR:
  RJ=1:GOSUB DO_READ_CHAR
  RETURN

REM PEEK_CHAR(A$, RI) -> C$
PEEK_CHAR:
  RJ=0:GOSUB DO_READ_CHAR
  RETURN

REM DO_READ_CHAR(RJ, A$, RI):
REM   - RI is position in A$
REM   - RJ=1 is read, RJ=0 is peek
DO_READ_CHAR:
  C$=""
  IF RF>0 THEN GOTO READ_FILE_CHAR
  IF RI<=LEN(A$) THEN C$=MID$(A$,RI,1):RI=RI+RJ
  RETURN

REM READ_FILE_CHAR(RJ) -> C$
REM   - RJ=1 is read, RJ=0 is peek
REM   - D$ is global used for already read pending character
REM   - EZ is global used for end of file state
READ_FILE_CHAR:
  IF D$<>"" THEN C$=D$:IF RJ=0 THEN RETURN
  IF D$<>"" AND RJ=1 THEN D$="":RETURN
  D$=""
  IF EZ>2 THEN C$=""
  IF EZ=2 THEN C$=")"
  IF EZ=1 THEN C$=CHR$(10)
  IF EZ>0 THEN EZ=EZ+RJ:RETURN
  #cbm GET#2,C$
  #qbasic C$=INPUT$(1,2)
  #qbasic IF EOF(2) THEN EZ=1:RETURN
  IF RJ=0 THEN D$=C$
  #cbm IF (ST AND 64) THEN EZ=1:RETURN
  #cbm IF (ST AND 255) THEN EZ=1:ER=-1:E$="File read error"+STR$(ST)
  RETURN

SKIP_SPACES:
  GOSUB PEEK_CHAR: REM peek at next character
  IF C$=" " OR C$="," OR C$=CHR$(13) OR C$=CHR$(10) THEN GOSUB READ_CHAR:GOTO SKIP_SPACES
  RETURN

SKIP_TO_EOL:
  GOSUB READ_CHAR
  IF C$="" OR C$=CHR$(13) OR C$=CHR$(10) THEN RETURN
  GOTO SKIP_TO_EOL


REM READ_FORM(A$, RI, RF) -> R
SUB READ_FORM
  Q=T:GOSUB PUSH_Q: REM save current value of T
  READ_FORM_RECUR:
  IF ER<>-2 THEN GOTO READ_FORM_RETURN
  GOSUB READ_TOKEN
  REM PRINT "READ_FORM T$: ["+T$+"]"
  IF T$="" THEN R=0:GOSUB INC_REF_R:GOTO READ_FORM_RETURN
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
  IF C$="(" THEN T=6:Q=41:GOTO READ_SEQ_START: REM ")"
  IF C$="[" THEN T=7:Q=93:GOTO READ_SEQ_START: REM "]"
  IF C$="{" THEN T=8:Q=125:GOTO READ_SEQ_START: REM "}"
  IF C$=")" OR C$="]" OR C$="}" THEN R=-1:ER=-1:E$="unexpected "+C$:GOTO READ_FORM_RETURN
  GOTO READ_SYMBOL

  READ_NIL_BOOL:
    REM PRINT "READ_NIL_BOOL"
    R=T*2
    GOSUB INC_REF_R
    GOTO READ_FORM_RETURN
  READ_NUMBER:
    REM PRINT "READ_NUMBER"
    T=2:L=VAL(T$):GOSUB ALLOC
    GOTO READ_FORM_RETURN
  READ_MACRO:
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
    IF C<>34 THEN R=-1:ER=-1:E$="expected '"+CHR$(34)+"', got EOF":GOTO READ_FORM_RETURN
    J=2:R$=""
    READ_STRING_LOOP:
    #qbasic I=INSTR(J,T$,CHR$(92))
    #cbm I=J
    #cbm INSTR_LOOP:
    #cbm IF I>LEN(T$) THEN I=0:GOTO INSTR_DONE
    #cbm IF MID$(T$,I,1)=CHR$(92) THEN GOTO INSTR_DONE
    #cbm I=I+1
    #cbm GOTO INSTR_LOOP
    #cbm INSTR_DONE:
    IF I=0 THEN GOTO READ_STRING_DONE
    R$=R$+MID$(T$,J,I-J)
    C$=MID$(T$,I+1,1)
    #qbasic IF C$="n" THEN R$=R$+CHR$(10) ELSE R$=R$+C$
    #cbm IF C$="n" THEN R$=R$+CHR$(13)
    #cbm IF C$<>"n" THEN R$=R$+C$
    J=I+2
    GOTO READ_STRING_LOOP
    READ_STRING_DONE:
    IF J=LEN(T$)+1 THEN R=-1:ER=-1:E$="expected '"+CHR$(34)+"', got EOF":GOTO READ_FORM_RETURN
    R$=R$+MID$(T$,J,LEN(T$)-J)
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
    SD=SD+1

    GOSUB PUSH_Q: REM push return character

    REM setup the stack for the loop, T has type
    GOSUB MAP_LOOP_START

  READ_SEQ_LOOP:

    REM TODO: reduce redundancy with READ_TOKEN
    GOSUB SKIP_SPACES
    GOSUB PEEK_CHAR: REM peek at next character
    IF C$="" THEN ER=-1:E$="unexpected EOF":GOTO READ_SEQ_DONE
    IF C$=";" THEN GOSUB SKIP_TO_EOL:GOTO READ_SEQ_LOOP
    Q=3:GOSUB PEEK_Q_Q
    IF C$=CHR$(Q) THEN GOSUB READ_CHAR:GOTO READ_SEQ_DONE

    CALL READ_FORM
    M=R: REM value (or key for hash-maps)

    REM if error, release the unattached element
    IF ER<>-2 THEN AY=R:GOSUB RELEASE:GOTO READ_SEQ_DONE

    REM if this is a hash-map, READ_FORM again
    IF T=8 THEN GOSUB PUSH_R:CALL READ_FORM
    IF T=8 THEN N=R:GOSUB POP_Q:M=Q: REM set key and value

    REM update the return sequence structure
    REM release N since list takes full ownership
    C=1:GOSUB MAP_LOOP_UPDATE

    GOTO READ_SEQ_LOOP

  READ_SEQ_DONE:
    SD=SD-1
    REM cleanup stack and get return value
    GOSUB MAP_LOOP_DONE

    GOSUB POP_Q: REM pop end character ptr
    GOTO READ_FORM_RETURN

  READ_FORM_RETURN:
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
  RF=1: REM reading from file
  EZ=0: REM file read state (1: EOF)
  SD=0: REM sequence read depth
  D$="": REM pending read/peek character
  #cbm OPEN 2,8,0,A$
  #qbasic IF NOT _FILEEXISTS(A$) THEN ER=-1:E$="File not found":RETURN
  #qbasic OPEN A$ FOR INPUT AS #2
  REM READ_TOKEN adds "(do ... )"
  CALL READ_FORM
  CLOSE 2
  EZ=0
  RETURN
