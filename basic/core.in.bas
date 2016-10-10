
REM DO_FUNCTION(F%, AR%)
DO_FUNCTION:
  REM Get the function number
  FF%=Z%(F%,1)

  REM Get argument values
  R%=AR%+1:GOSUB DEREF_R:AA%=R%
  R%=Z%(AR%,1)+1:GOSUB DEREF_R:AB%=R%

  REM Switch on the function number
  IF FF%>=61 THEN ER%=-1:ER$="unknown function"+STR$(FF%):RETURN
  IF FF%>=53 THEN DO_53
  IF FF%>=39 THEN DO_39
  IF FF%>=27 THEN DO_27
  IF FF%>=18 THEN DO_18
  IF FF%>=11 THEN DO_11

  ON FF% GOTO DO_EQUAL_Q,DO_THROW,DO_NIL_Q,DO_TRUE_Q,DO_FALSE_Q,DO_STRING_Q,DO_SYMBOL,DO_SYMBOL_Q
  DO_11:
  ON FF%-10 GOTO DO_PR_STR,DO_STR,DO_PRN,DO_PRINTLN,DO_READ_STRING,DO_READLINE,DO_SLURP
  DO_18:
  ON FF%-17 GOTO DO_LT,DO_LTE,DO_GT,DO_GTE,DO_ADD,DO_SUB,DO_MULT,DO_DIV,DO_TIME_MS
  DO_27:
  ON FF%-26 GOTO DO_LIST,DO_LIST_Q,DO_VECTOR,DO_VECTOR_Q,DO_HASH_MAP,DO_MAP_Q
  DO_39:
  ON FF%-38 GOTO DO_SEQUENTIAL_Q,DO_CONS,DO_CONCAT,DO_NTH,DO_FIRST,DO_REST,DO_EMPTY_Q,DO_COUNT,DO_APPLY,DO_MAP
  DO_53:
  ON FF%-52 GOTO DO_ATOM,DO_ATOM_Q,DO_DEREF,DO_RESET_BANG,DO_SWAP_BANG,DO_PR_MEMORY,DO_PR_MEMORY_SUMMARY,DO_EVAL

  DO_EQUAL_Q:
    A%=AA%:B%=AB%:GOSUB EQUAL_Q
    R%=R%+1
    RETURN
  DO_THROW:
    ER%=AA%
    Z%(ER%,0)=Z%(ER%,0)+16
    R%=0
    RETURN
  DO_NIL_Q:
    R%=1
    IF AA%=0 THEN R%=2
    RETURN
  DO_TRUE_Q:
    R%=1
    IF AA%=2 THEN R%=2
    RETURN
  DO_FALSE_Q:
    R%=1
    IF AA%=1 THEN R%=2
    RETURN
  DO_STRING_Q:
    R%=1
    IF (Z%(AA%,0)AND15)=4 THEN R%=2
    RETURN
  DO_SYMBOL:
    R%=0
    RETURN
  DO_SYMBOL_Q:
    R%=1
    IF (Z%(AA%,0)AND15)=5 THEN R%=2
    RETURN

  DO_PR_STR:
    AZ%=AR%:PR%=1:SE$=" ":GOSUB PR_STR_SEQ
    AS$=R$:T%=4+16:GOSUB STRING
    RETURN
  DO_STR:
    AZ%=AR%:PR%=0:SE$="":GOSUB PR_STR_SEQ
    AS$=R$:T%=4+16:GOSUB STRING
    RETURN
  DO_PRN:
    AZ%=AR%:PR%=1:SE$=" ":GOSUB PR_STR_SEQ
    PRINT R$
    R%=0
    RETURN
  DO_PRINTLN:
    AZ%=AR%:PR%=0:SE$=" ":GOSUB PR_STR_SEQ
    PRINT R$
    R%=0
    RETURN
  DO_READ_STRING:
    A$=ZS$(Z%(AA%,1))
    GOSUB READ_STR
    RETURN
  DO_READLINE:
    A$=ZS$(Z%(AA%,1)):GOSUB READLINE
    IF EOF=1 THEN EOF=0:R%=0:RETURN
    AS$=R$:T%=4:GOSUB STRING
    Z%(R%,0)=Z%(R%,0)+16
    RETURN
  DO_SLURP:
    R$=""
    REM OPEN 1,8,2,ZS$(Z%(AA%,1))+",SEQ,R"
    REM OPEN 1,8,2,ZS$(Z%(AA%,1))
    OPEN 1,8,0,ZS$(Z%(AA%,1))
    DO_SLURP_LOOP:
      A$=""
      GET#1,A$
      IF ASC(A$)=10 THEN R$=R$+CHR$(13)
      IF (ASC(A$)<>10) AND (A$<>"") THEN R$=R$+A$
      IF (ST AND 64) THEN GOTO DO_SLURP_DONE
      IF (ST AND 255) THEN ER%=-1:ER$="File read error "+STR$(ST):RETURN
      GOTO DO_SLURP_LOOP
    DO_SLURP_DONE:
      CLOSE 1
      AS$=R$:T%=4+16:GOSUB STRING
      RETURN

  DO_LT:
    R%=1
    IF Z%(AA%,1)<Z%(AB%,1) THEN R%=2
    RETURN
  DO_LTE:
    R%=1
    IF Z%(AA%,1)<=Z%(AB%,1) THEN R%=2
    RETURN
  DO_GT:
    R%=1
    IF Z%(AA%,1)>Z%(AB%,1) THEN R%=2
    RETURN
  DO_GTE:
    R%=1
    IF Z%(AA%,1)>=Z%(AB%,1) THEN R%=2
    RETURN

  DO_ADD:
    SZ%=1:GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)+Z%(AB%,1)
    RETURN
  DO_SUB:
    SZ%=1:GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)-Z%(AB%,1)
    RETURN
  DO_MULT:
    SZ%=1:GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)*Z%(AB%,1)
    RETURN
  DO_DIV:
    SZ%=1:GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)/Z%(AB%,1)
    RETURN
  DO_TIME_MS:
    R%=0
    RETURN

  DO_LIST:
    R%=AR%
    Z%(R%,0)=Z%(R%,0)+16
    RETURN
  DO_LIST_Q:
    A%=AA%:GOSUB LIST_Q
    R%=R%+1: REM map to mal false/true
    RETURN
  DO_VECTOR:
    R%=0
    RETURN
  DO_VECTOR_Q:
    R%=1
    IF (Z%(AA%,0)AND15)=7 THEN R%=2
    RETURN
  DO_HASH_MAP:
    R%=0
    RETURN
  DO_MAP_Q:
    R%=1
    IF (Z%(AA%,0)AND15)=8 THEN R%=2
    RETURN

  DO_SEQUENTIAL_Q:
    R%=1
    IF (Z%(AA%,0)AND15)=6 OR (Z%(AA%,0)AND15)=7 THEN R%=2
    RETURN
  DO_CONS:
    A%=AA%:B%=AB%:GOSUB CONS
    RETURN
  DO_CONCAT:
    REM if empty arguments, return empty list
    IF Z%(AR%,1)=0 THEN R%=3:Z%(R%,0)=Z%(R%,0)+16:RETURN

    REM single argument
    IF Z%(Z%(AR%,1),1)<>0 THEN GOTO DO_CONCAT_MULT
      REM if single argument and it's a list, return it
      IF (Z%(AA%,0)AND15)=6 THEN R%=AA%:Z%(R%,0)=Z%(R%,0)+16:RETURN
      REM otherwise, copy first element to turn it into a list
      B%=AA%+1:GOSUB DEREF_B: REM value to copy
      SZ%=2:GOSUB ALLOC
      Z%(R%,0)=6+16:Z%(R%,1)=Z%(AA%,1)
      Z%(R%+1,0)=14:Z%(R%+1,1)=B%
      REM inc ref count of trailing list part and of copied value
      Z%(Z%(AA%,1),0)=Z%(Z%(AA%,1),0)+16
      Z%(B%,0)=Z%(B%,0)+16
      RETURN

    REM multiple arguments
    DO_CONCAT_MULT:
      CZ%=ZL%: REM save current stack position
      REM push arguments onto the stack
      DO_CONCAT_STACK:
        R%=AR%+1:GOSUB DEREF_R
        ZL%=ZL%+1:ZZ%(ZL%)=R%: REM push sequence
        AR%=Z%(AR%,1)
        IF Z%(AR%,1)<>0 THEN GOTO DO_CONCAT_STACK

    REM pop last argument as our seq to prepend to
    AB%=ZZ%(ZL%):ZL%=ZL%-1
    REM last arg/seq is not copied so we need to inc ref to it
    Z%(AB%,0)=Z%(AB%,0)+16
    DO_CONCAT_LOOP:
      IF ZL%=CZ% THEN R%=AB%:RETURN
      AA%=ZZ%(ZL%):ZL%=ZL%-1: REM pop off next seq to prepend
      IF Z%(AA%,1)=0 THEN GOTO DO_CONCAT_LOOP: REM skip empty seqs
      A%=AA%:B%=0:C%=-1:GOSUB SLICE

      REM release the terminator of new list (we skip over it)
      AY%=Z%(R6%,1):GOSUB RELEASE
      REM attach new list element before terminator (last actual
      REM element to the next sequence
      Z%(R6%,1)=AB%

      AB%=R%
      GOTO DO_CONCAT_LOOP
  DO_NTH:
    B%=Z%(AB%,1)
    A%=AA%:GOSUB COUNT
    IF R%<=B% THEN R%=0:ER%=-1:ER$="nth: index out of range":RETURN
    DO_NTH_LOOP:
      IF B%=0 THEN GOTO DO_NTH_DONE
      B%=B%-1
      AA%=Z%(AA%,1)
      GOTO DO_NTH_LOOP
    DO_NTH_DONE:
      R%=Z%(AA%+1,1)
      Z%(R%,0)=Z%(R%,0)+16
      RETURN
  DO_FIRST:
    IF Z%(AA%,1)=0 THEN R%=0
    IF Z%(AA%,1)<>0 THEN R%=AA%+1:GOSUB DEREF_R
    IF R%<>0 THEN Z%(R%,0)=Z%(R%,0)+16
    RETURN
  DO_REST:
    IF Z%(AA%,1)=0 THEN R%=AA%
    IF Z%(AA%,1)<>0 THEN R%=Z%(AA%,1)
    Z%(R%,0)=Z%(R%,0)+16
    RETURN
  DO_EMPTY_Q:
    R%=1
    IF Z%(AA%,1)=0 THEN R%=2
    RETURN
  DO_COUNT:
    A%=AA%:GOSUB COUNT:R4%=R%
    SZ%=1:GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=R4%
    RETURN
  DO_APPLY:
    F%=AA%
    AR%=Z%(AR%,1)
    A%=AR%:GOSUB COUNT:R4%=R%

    REM no intermediate args, just call APPLY directly
    IF R4%<=1 THEN AR%=Z%(AR%+1,1):GOSUB APPLY:RETURN

    REM prepend intermediate args to final args element
    A%=AR%:B%=0:C%=R4%-1:GOSUB SLICE
    REM release the terminator of new list (we skip over it)
    AY%=Z%(R6%,1):GOSUB RELEASE
    REM attach end of slice to final args element
    Z%(R6%,1)=Z%(A%+1,1)
    Z%(Z%(A%+1,1),0)=Z%(Z%(A%+1,1),0)+16

    ZL%=ZL%+1:ZZ%(ZL%)=R%: REM push/save new args for release
    AR%=R%:GOSUB APPLY
    AY%=ZZ%(ZL%):ZL%=ZL%-1:GOSUB RELEASE: REM pop/release new args
    RETURN
  DO_MAP:
    F%=AA%

    REM first result list element
    SZ%=2:GOSUB ALLOC

    REM push future return val, prior entry, F% and AB%
    ZL%=ZL%+4:ZZ%(ZL%-3)=R%:ZZ%(ZL%-2)=0:ZZ%(ZL%-1)=F%:ZZ%(ZL%)=AB%

    DO_MAP_LOOP:
      REM set base values
      Z%(R%,0)=6+16:Z%(R%,1)=0
      Z%(R%+1,0)=14:Z%(R%+1,1)=0

      REM set previous to current if not the first element
      IF ZZ%(ZL%-2)<>0 THEN Z%(ZZ%(ZL%-2),1)=R%
      REM update previous reference to current
      ZZ%(ZL%-2)=R%

      IF Z%(AB%,1)=0 THEN GOTO DO_MAP_DONE

      REM create argument list for apply call
      SZ%=2:GOSUB ALLOC
      Z%(R%,0)=6+16:Z%(R%,1)=0
      Z%(R%+1,0)=14:Z%(R%+1,1)=0
      AR%=R%: REM save end of list temporarily
      SZ%=2:GOSUB ALLOC
      Z%(R%,0)=6+16:Z%(R%,1)=AR%
      REM inc ref cnt of referred argument
      A%=Z%(AB%+1,1): Z%(A%,0)=Z%(A%,0)+16
      Z%(R%+1,0)=14:Z%(R%+1,1)=A%

      REM push argument list
      ZL%=ZL%+1:ZZ%(ZL%)=R%

      AR%=R%:GOSUB APPLY

      REM pop apply args are release them
      AY%=ZZ%(ZL%):ZL%=ZL%-1:GOSUB RELEASE

      REM set the result value
      Z%(ZZ%(ZL%-2)+1,1)=R%

      REM restore F%
      F%=ZZ%(ZL%-1)

      REM update AB% to next source element
      ZZ%(ZL%)=Z%(ZZ%(ZL%),1)
      AB%=ZZ%(ZL%)

      REM allocate next element
      SZ%=2:GOSUB ALLOC

      GOTO DO_MAP_LOOP

    DO_MAP_DONE:
      REM get return val
      R%=ZZ%(ZL%-3)
      REM pop everything off stack
      ZL%=ZL%-4
      RETURN

  DO_ATOM:
    SZ%=1:GOSUB ALLOC
    Z%(AA%,0)=Z%(AA%,0)+16: REM inc ref cnt of contained value
    Z%(R%,0)=12+16
    Z%(R%,1)=AA%
    RETURN
  DO_ATOM_Q:
    R%=1
    IF (Z%(AA%,0)AND15)=12 THEN R%=2
    RETURN
  DO_DEREF:
    R%=Z%(AA%,1):GOSUB DEREF_R
    Z%(R%,0)=Z%(R%,0)+16
    RETURN
  DO_RESET_BANG:
    R%=AB%
    REM release current value
    AY%=Z%(AA%,1):GOSUB RELEASE
    REM inc ref by 2 for atom ownership and since we are returning it
    Z%(R%,0)=Z%(R%,0)+32
    REM update value
    Z%(AA%,1)=R%
    RETURN
  DO_SWAP_BANG:
    F%=AB%

    REM add atom to front of the args list
    A%=Z%(AA%,1):B%=Z%(Z%(AR%,1),1):GOSUB CONS
    AR%=R%

    REM push args for release after
    ZL%=ZL%+1:ZZ%(ZL%)=AR%

    REM push atom
    ZL%=ZL%+1:ZZ%(ZL%)=AA%

    GOSUB APPLY

    REM pop atom
    AA%=ZZ%(ZL%):ZL%=ZL%-1

    REM pop and release args
    AY%=ZZ%(ZL%):ZL%=ZL%-1:GOSUB RELEASE

    REM use reset to update the value
    AB%=R%:GOSUB DO_RESET_BANG

    REM but decrease ref cnt of return by 1 (not sure why)
    AY%=R%:GOSUB RELEASE

    RETURN

  DO_PR_MEMORY:
    P1%=ZT%:P2%=-1:GOSUB PR_MEMORY
    RETURN
  DO_PR_MEMORY_SUMMARY:
    GOSUB PR_MEMORY_SUMMARY
    RETURN

  DO_EVAL:
    A%=AA%:E%=RE%:GOSUB EVAL
    RETURN

INIT_CORE_SET_FUNCTION:
  GOSUB NATIVE_FUNCTION
  V%=R%:GOSUB ENV_SET_S
  RETURN

REM INIT_CORE_NS(E%)
INIT_CORE_NS:
  REM create the environment mapping
  REM must match DO_FUNCTION mappings

  K$="=":A%=1:GOSUB INIT_CORE_SET_FUNCTION
  K$="throw":A%=2:GOSUB INIT_CORE_SET_FUNCTION
  K$="nil?":A%=3:GOSUB INIT_CORE_SET_FUNCTION
  K$="true?":A%=4:GOSUB INIT_CORE_SET_FUNCTION
  K$="false?":A%=5:GOSUB INIT_CORE_SET_FUNCTION
  K$="string?":A%=6:GOSUB INIT_CORE_SET_FUNCTION
  K$="symbol":A%=7:GOSUB INIT_CORE_SET_FUNCTION
  K$="symbol?":A%=8:GOSUB INIT_CORE_SET_FUNCTION

  K$="pr-str":A%=11:GOSUB INIT_CORE_SET_FUNCTION
  K$="str":A%=12:GOSUB INIT_CORE_SET_FUNCTION
  K$="prn":A%=13:GOSUB INIT_CORE_SET_FUNCTION
  K$="println":A%=14:GOSUB INIT_CORE_SET_FUNCTION
  K$="read-string":A%=15:GOSUB INIT_CORE_SET_FUNCTION
  K$="readline":A%=16:GOSUB INIT_CORE_SET_FUNCTION
  K$="slurp":A%=17:GOSUB INIT_CORE_SET_FUNCTION

  K$="<":A%=18:GOSUB INIT_CORE_SET_FUNCTION
  K$="<=":A%=19:GOSUB INIT_CORE_SET_FUNCTION
  K$=">":A%=20:GOSUB INIT_CORE_SET_FUNCTION
  K$=">=":A%=21:GOSUB INIT_CORE_SET_FUNCTION
  K$="+":A%=22:GOSUB INIT_CORE_SET_FUNCTION
  K$="-":A%=23:GOSUB INIT_CORE_SET_FUNCTION
  K$="*":A%=24:GOSUB INIT_CORE_SET_FUNCTION
  K$="/":A%=25:GOSUB INIT_CORE_SET_FUNCTION
  K$="time-ms":A%=26:GOSUB INIT_CORE_SET_FUNCTION

  K$="list":A%=27:GOSUB INIT_CORE_SET_FUNCTION
  K$="list?":A%=28:GOSUB INIT_CORE_SET_FUNCTION
  K$="vector":A%=29:GOSUB INIT_CORE_SET_FUNCTION
  K$="vector?":A%=30:GOSUB INIT_CORE_SET_FUNCTION
  K$="hash-map":A%=31:GOSUB INIT_CORE_SET_FUNCTION
  K$="map?":A%=32:GOSUB INIT_CORE_SET_FUNCTION

  K$="sequential?":A%=39:GOSUB INIT_CORE_SET_FUNCTION
  K$="cons":A%=40:GOSUB INIT_CORE_SET_FUNCTION
  K$="concat":A%=41:GOSUB INIT_CORE_SET_FUNCTION
  K$="nth":A%=42:GOSUB INIT_CORE_SET_FUNCTION
  K$="first":A%=43:GOSUB INIT_CORE_SET_FUNCTION
  K$="rest":A%=44:GOSUB INIT_CORE_SET_FUNCTION
  K$="empty?":A%=45:GOSUB INIT_CORE_SET_FUNCTION
  K$="count":A%=46:GOSUB INIT_CORE_SET_FUNCTION
  K$="apply":A%=47:GOSUB INIT_CORE_SET_FUNCTION
  K$="map":A%=48:GOSUB INIT_CORE_SET_FUNCTION

  K$="atom":A%=53:GOSUB INIT_CORE_SET_FUNCTION
  K$="atom?":A%=54:GOSUB INIT_CORE_SET_FUNCTION
  K$="deref":A%=55:GOSUB INIT_CORE_SET_FUNCTION
  K$="reset!":A%=56:GOSUB INIT_CORE_SET_FUNCTION
  K$="swap!":A%=57:GOSUB INIT_CORE_SET_FUNCTION

  K$="pr-memory":A%=58:GOSUB INIT_CORE_SET_FUNCTION
  K$="pr-memory-summary":A%=59:GOSUB INIT_CORE_SET_FUNCTION
  K$="eval":A%=60:GOSUB INIT_CORE_SET_FUNCTION

  RETURN
