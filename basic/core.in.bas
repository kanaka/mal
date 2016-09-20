
REM DO_FUNCTION(F%, AR%)
DO_FUNCTION:
  REM Get the function number
  FF%=Z%(F%,1)

  REM Get argument values
  R%=AR%+1: GOSUB DEREF_R: AA%=R%
  R%=Z%(AR%,1)+1: GOSUB DEREF_R: AB%=R%

  REM Switch on the function number
  IF FF%=1 THEN DO_EQUAL_Q

  IF FF%=11 THEN DO_PR_STR
  IF FF%=12 THEN DO_STR
  IF FF%=13 THEN DO_PRN
  IF FF%=14 THEN DO_PRINTLN
  IF FF%=16 THEN DO_READ_STRING
  IF FF%=17 THEN DO_SLURP

  IF FF%=18 THEN DO_LT
  IF FF%=19 THEN DO_LTE
  IF FF%=20 THEN DO_GT
  IF FF%=21 THEN DO_GTE
  IF FF%=22 THEN DO_ADD
  IF FF%=23 THEN DO_SUB
  IF FF%=24 THEN DO_MULT
  IF FF%=25 THEN DO_DIV

  IF FF%=27 THEN DO_LIST
  IF FF%=28 THEN DO_LIST_Q

  IF FF%=39 THEN DO_CONS
  IF FF%=43 THEN DO_FIRST
  IF FF%=44 THEN DO_REST
  IF FF%=45 THEN DO_EMPTY_Q
  IF FF%=46 THEN DO_COUNT

  IF FF%=53 THEN DO_ATOM
  IF FF%=54 THEN DO_ATOM_Q
  IF FF%=55 THEN DO_DEREF
  IF FF%=56 THEN DO_RESET_BANG
  IF FF%=57 THEN DO_SWAP_BANG

  IF FF%=58 THEN DO_PR_MEMORY
  IF FF%=59 THEN DO_PR_MEMORY_SUMMARY
  IF FF%=60 THEN DO_EVAL
  ER%=1: ER$="unknown function" + STR$(FF%): RETURN

  DO_EQUAL_Q:
    A%=AA%: B%=AB%: GOSUB EQUAL_Q
    R%=R%+1
    RETURN

  DO_PR_STR:
    AZ%=AR%: PR%=1: SE$=" ": GOSUB PR_STR_SEQ
    AS$=R$: GOSUB STRING
    R4%=R%
    SZ%=1: GOSUB ALLOC
    Z%(R%,0) = 4+16
    Z%(R%,1) = R4%
    RETURN
  DO_STR:
    AZ%=AR%: PR%=0: SE$="": GOSUB PR_STR_SEQ
    AS$=R$: GOSUB STRING
    R4%=R%
    SZ%=1: GOSUB ALLOC
    Z%(R%,0) = 4+16
    Z%(R%,1) = R4%
    RETURN
  DO_PRN:
    AZ%=AR%: PR%=1: SE$=" ": GOSUB PR_STR_SEQ
    PRINT R$
    R%=0
    RETURN
  DO_PRINTLN:
    AZ%=AR%: PR%=0: SE$=" ": GOSUB PR_STR_SEQ
    PRINT R$
    R%=0
    RETURN
  DO_READ_STRING:
    A$=ZS$(Z%(AA%,1))
    GOSUB READ_STR
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
      IF (ST AND 255) THEN ER%=-1: ER%="File read error "+STR$(ST): RETURN
      GOTO DO_SLURP_LOOP
    DO_SLURP_DONE:
      CLOSE 1
      AS$=R$: GOSUB STRING
      R4%=R%
      SZ%=1: GOSUB ALLOC
      Z%(R%,0) = 4+16
      Z%(R%,1) = R4%
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
    SZ%=1: GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)+Z%(AB%,1)
    RETURN
  DO_SUB:
    SZ%=1: GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)-Z%(AB%,1)
    RETURN
  DO_MULT:
    SZ%=1: GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)*Z%(AB%,1)
    RETURN
  DO_DIV:
    SZ%=1: GOSUB ALLOC
    Z%(R%,0)=2+16
    Z%(R%,1)=Z%(AA%,1)/Z%(AB%,1)
    RETURN

  DO_LIST:
    R%=AR%
    Z%(R%,0)=Z%(R%,0)+16
    RETURN
  DO_LIST_Q:
    A%=AA%: GOSUB LIST_Q
    R%=R%+1: REM map to mal false/true
    RETURN

  DO_CONS:
    A%=AA%: B%=AB%: GOSUB CONS
    RETURN
  DO_FIRST:
    IF Z%(AA%,1)=0 THEN R%=0
    IF Z%(AA%,1)<>0 THEN R%=AA%+1: GOSUB DEREF_R
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
    A%=AA%: GOSUB COUNT
    R4%=R%
    SZ%=1: GOSUB ALLOC
    Z%(R%,0) = 2+16
    Z%(R%,1) = R4%
    RETURN

  DO_ATOM:
    SZ%=1: GOSUB ALLOC
    Z%(AA%,0)=Z%(AA%,0)+16: REM inc ref cnt of contained value
    Z%(R%,0) = 11+16
    Z%(R%,1) = AA%
    RETURN
  DO_ATOM_Q:
    R%=1
    IF (Z%(AA%,0)AND15)=11 THEN R%=2
    RETURN
  DO_DEREF:
    R%=Z%(AA%,1): GOSUB DEREF_R
    Z%(R%,0)=Z%(R%,0)+16
    RETURN
  DO_RESET_BANG:
    R%=AB%
    REM release current value
    AY%=Z%(AA%,1): GOSUB RELEASE
    REM inc ref by 2 for atom ownership and since we are returning it
    Z%(R%,0)=Z%(R%,0)+32
    REM update value
    Z%(AA%,1)=R%
    RETURN
  DO_SWAP_BANG:
    F%=AB%

    REM add atom to front of the args list
    A%=Z%(AA%,1): B%=Z%(Z%(AR%,1),1): GOSUB CONS
    AR%=R%

    REM push args for release after
    ZL%=ZL%+1: ZZ%(ZL%)=AR%

    REM TODO: break this out into APPLY
    IF (Z%(F%,0)AND15)=9 THEN GOTO DO_SWAP_FUNCTION
    IF (Z%(F%,0)AND15)=10 THEN GOTO DO_SWAP_MAL_FUNCTION

    DO_SWAP_FUNCTION:
      REM push atom
      ZL%=ZL%+1: ZZ%(ZL%)=AA%

      GOSUB DO_FUNCTION

      REM pop atom
      AA%=ZZ%(ZL%): ZL%=ZL%-1

      REM pop and release args
      AY%=ZZ%(ZL%): ZL%=ZL%-1: GOSUB RELEASE
      
      GOTO DO_SWAP_DONE

    DO_SWAP_MAL_FUNCTION:
      REM push current environment for later release
      ZL%=ZL%+1: ZZ%(ZL%)=E%

      REM create new environ using env stored with function
      EO%=Z%(F%+1,1): BI%=Z%(F%+1,0): EX%=AR%: GOSUB ENV_NEW_BINDS

      REM push atom
      ZL%=ZL%+1: ZZ%(ZL%)=AA%

      A%=Z%(F%,1): E%=R%: GOSUB EVAL

      REM pop atom
      AA%=ZZ%(ZL%): ZL%=ZL%-1

      REM pop and release args
      AY%=ZZ%(ZL%): ZL%=ZL%-1: GOSUB RELEASE

      REM pop and release previous env
      AY%=ZZ%(ZL%): ZL%=ZL%-1: GOSUB RELEASE

      GOTO DO_SWAP_DONE

    DO_SWAP_DONE:
      REM use reset to update the value
      AB%=R%: GOSUB DO_RESET_BANG
      REM but decrease ref cnt of return by 1 (not sure why)
      AY%=R%: GOSUB RELEASE
      RETURN

  DO_PR_MEMORY:
    P1%=ZT%: P2%=-1: GOSUB PR_MEMORY
    RETURN
  DO_PR_MEMORY_SUMMARY:
    GOSUB PR_MEMORY_SUMMARY
    RETURN

  DO_EVAL:
    AZ%=AA%: PR%=1: GOSUB PR_STR
    A%=AA%: E%=RE%: GOSUB EVAL
    RETURN

INIT_CORE_SET_FUNCTION:
  GOSUB NATIVE_FUNCTION
  V%=R%: GOSUB ENV_SET_S
  RETURN

REM INIT_CORE_NS(E%)
INIT_CORE_NS:
  REM create the environment mapping
  REM must match DO_FUNCTION mappings

  K$="=": A%=1: GOSUB INIT_CORE_SET_FUNCTION

  K$="pr-str": A%=11: GOSUB INIT_CORE_SET_FUNCTION
  K$="str": A%=12: GOSUB INIT_CORE_SET_FUNCTION
  K$="prn": A%=13: GOSUB INIT_CORE_SET_FUNCTION
  K$="println": A%=14: GOSUB INIT_CORE_SET_FUNCTION
  K$="read-string": A%=16: GOSUB INIT_CORE_SET_FUNCTION
  K$="slurp": A%=17: GOSUB INIT_CORE_SET_FUNCTION

  K$="<":  A%=18: GOSUB INIT_CORE_SET_FUNCTION
  K$="<=": A%=19: GOSUB INIT_CORE_SET_FUNCTION
  K$=">":  A%=20: GOSUB INIT_CORE_SET_FUNCTION
  K$=">=": A%=21: GOSUB INIT_CORE_SET_FUNCTION
  K$="+":  A%=22: GOSUB INIT_CORE_SET_FUNCTION
  K$="-":  A%=23: GOSUB INIT_CORE_SET_FUNCTION
  K$="*":  A%=24: GOSUB INIT_CORE_SET_FUNCTION
  K$="/":  A%=25: GOSUB INIT_CORE_SET_FUNCTION

  K$="list": A%=27: GOSUB INIT_CORE_SET_FUNCTION
  K$="list?": A%=28: GOSUB INIT_CORE_SET_FUNCTION

  K$="cons": A%=39: GOSUB INIT_CORE_SET_FUNCTION
  K$="first": A%=43: GOSUB INIT_CORE_SET_FUNCTION
  K$="rest": A%=44: GOSUB INIT_CORE_SET_FUNCTION
  K$="empty?": A%=45: GOSUB INIT_CORE_SET_FUNCTION
  K$="count": A%=46: GOSUB INIT_CORE_SET_FUNCTION

  K$="atom": A%=53: GOSUB INIT_CORE_SET_FUNCTION
  K$="atom?": A%=54: GOSUB INIT_CORE_SET_FUNCTION
  K$="deref": A%=55: GOSUB INIT_CORE_SET_FUNCTION
  K$="reset!": A%=56: GOSUB INIT_CORE_SET_FUNCTION
  K$="swap!": A%=57: GOSUB INIT_CORE_SET_FUNCTION

  K$="pr-memory": A%=58: GOSUB INIT_CORE_SET_FUNCTION
  K$="pr-memory-summary": A%=59: GOSUB INIT_CORE_SET_FUNCTION
  K$="eval": A%=60: GOSUB INIT_CORE_SET_FUNCTION

  RETURN
