REM APPLY should really be in types.in.bas but it is here because it
REM calls DO_TCO_FUNCTION so it will cause syntax errors for steps1-3
REM if it is in types.in.bas because there are unresolved labels.

REM APPLY(F, AR) -> R
REM   - restores E
REM   - call using GOTO and with return label/address on the stack
SUB APPLY
  REM if metadata, get the actual object
  IF (Z%(F)AND 31)=14 THEN F=Z%(F+1)

  ON (Z%(F)AND 31)-8 GOTO APPLY_FUNCTION,APPLY_MAL_FUNCTION,APPLY_MAL_FUNCTION

  APPLY_FUNCTION:
    REM regular function
    IF Z%(F+1)<60 THEN GOSUB DO_FUNCTION:GOTO APPLY_DONE
    REM for recur functions (apply, map, swap!), use GOTO
    IF Z%(F+1)>60 THEN CALL DO_TCO_FUNCTION
    GOTO APPLY_DONE

  APPLY_MAL_FUNCTION:
    Q=E:GOSUB PUSH_Q: REM save the current environment

    REM create new environ using env and params stored in the
    REM function and bind the params to the apply arguments
    C=Z%(F+3):A=Z%(F+2):B=AR:GOSUB ENV_NEW_BINDS

    A=Z%(F+1):E=R:CALL EVAL

    AY=E:GOSUB RELEASE: REM release the new environment

    GOSUB POP_Q:E=Q: REM pop/restore the saved environment

  APPLY_DONE:
END SUB


REM DO_TCO_FUNCTION(F, AR)
SUB DO_TCO_FUNCTION
  G=Z%(F+1)

  REM Get argument values
  A=Z%(AR+2)
  B=Z%(Z%(AR+1)+2)

  ON G-60 GOTO DO_APPLY,DO_MAP,DO_SWAP_BANG

  DO_APPLY:
    F=A
    AR=Z%(AR+1)
    A=AR:GOSUB COUNT:C=R

    A=Z%(AR+2)
    REM no intermediate args, but not a list, so convert it first
    IF C<=1 AND (Z%(A)AND 31)<>6 THEN T=6:GOSUB FORCE_SEQ_TYPE:GOTO DO_APPLY_2
    REM no intermediate args, just call APPLY directly
    IF C<=1 THEN GOTO DO_APPLY_1

    REM prepend intermediate args to final args element
    A=AR:B=0:C=C-1:GOSUB SLICE
    REM release the terminator of new list (we skip over it)
    REM we already checked for an empty list above, so R6 is pointer
    REM a real non-empty list
    AY=Z%(R6+1):GOSUB RELEASE
    REM attach end of slice to final args element
    Z%(R6+1)=Z%(A+2)
    Z%(Z%(A+2))=Z%(Z%(A+2))+32

    GOTO DO_APPLY_2

    DO_APPLY_1:
      AR=A:CALL APPLY

      GOTO DO_TCO_FUNCTION_DONE

    DO_APPLY_2:
      GOSUB PUSH_R: REM push/save new args for release

      AR=R:CALL APPLY

      REM pop/release new args
      GOSUB POP_Q:AY=Q
      GOSUB RELEASE
      GOTO DO_TCO_FUNCTION_DONE

  DO_MAP:
    F=A

    REM setup the stack for the loop
    T=6:GOSUB MAP_LOOP_START

    DO_MAP_LOOP:
      IF Z%(B+1)=0 THEN GOTO DO_MAP_DONE

      REM create argument list for apply
      T=6:L=6:M=Z%(B+2):GOSUB ALLOC

      GOSUB PUSH_R: REM push argument list
      Q=F:GOSUB PUSH_Q: REM push F
      Q=B:GOSUB PUSH_Q: REM push B

      AR=R:CALL APPLY

      GOSUB POP_Q:B=Q: REM pop B
      GOSUB POP_Q:F=Q: REM pop F
      GOSUB POP_Q: REM pop apply args and release them
      AY=Q:GOSUB RELEASE

      REM main value is result of apply
      M=R

      B=Z%(B+1): REM go to the next element

      REM if error, release the unattached element
      IF ER<>-2 THEN AY=R:GOSUB RELEASE:GOTO DO_MAP_DONE

      REM update the return sequence structure
      REM release N since list takes full ownership
      C=1:T=6:GOSUB MAP_LOOP_UPDATE

      GOTO DO_MAP_LOOP

    DO_MAP_DONE:
      REM cleanup stack and get return value
      GOSUB MAP_LOOP_DONE
      GOTO DO_TCO_FUNCTION_DONE

  DO_SWAP_BANG:
    F=B

    REM add atom to front of the args list
    T=6:L=Z%(Z%(AR+1)+1):M=Z%(A+1):GOSUB ALLOC: REM cons
    AR=R

    REM push args for release after
    Q=AR:GOSUB PUSH_Q

    REM push atom
    Q=A:GOSUB PUSH_Q

    CALL APPLY

    REM pop atom
    GOSUB POP_Q:A=Q

    REM pop and release args
    GOSUB POP_Q:AY=Q
    GOSUB RELEASE

    REM use reset to update the value
    B=R:GOSUB DO_RESET_BANG

    REM but decrease ref cnt of return by 1 (not sure why)
    AY=R:GOSUB RELEASE

    GOTO DO_TCO_FUNCTION_DONE

  DO_TCO_FUNCTION_DONE:
END SUB

REM RETURN_INC_REF(R) -> R
REM   - return R with 1 ref cnt increase
REM   - called with GOTO as a return RETURN
RETURN_INC_REF:
  Z%(R)=Z%(R)+32
  RETURN

REM RETURN_TRUE_FALSE(R) -> R
REM   - take BASIC true/false R, return mal true/false R with ref cnt
REM   - called with GOTO as a return RETURN
RETURN_TRUE_FALSE:
  IF R THEN R=4
  IF R=0 THEN R=2
  GOTO RETURN_INC_REF

REM DO_FUNCTION(F, AR)
DO_FUNCTION:
  REM Get the function number
  G=Z%(F+1)

  REM Get argument values
  A=Z%(AR+2)
  B=Z%(Z%(AR+1)+2)

  REM Switch on the function number
  IF G>59 THEN ER=-1:E$="unknown function"+STR$(G):RETURN
  ON INT(G/10)+1 GOTO DO_1_9,DO_10_19,DO_20_29,DO_30_39,DO_40_49,DO_50_59

  DO_1_9:
  ON G GOTO DO_EQUAL_Q,DO_THROW,DO_NIL_Q,DO_TRUE_Q,DO_FALSE_Q,DO_STRING_Q,DO_SYMBOL,DO_SYMBOL_Q,DO_KEYWORD
  DO_10_19:
  ON G-9 GOTO DO_KEYWORD_Q,DO_PR_STR,DO_STR,DO_PRN,DO_PRINTLN,DO_READ_STRING,DO_READLINE,DO_SLURP,DO_LT,DO_LTE
  DO_20_29:
  ON G-19 GOTO DO_GT,DO_GTE,DO_ADD,DO_SUB,DO_MULT,DO_DIV,DO_TIME_MS,DO_LIST,DO_LIST_Q,DO_VECTOR
  DO_30_39:
  ON G-29 GOTO DO_VECTOR_Q,DO_HASH_MAP,DO_MAP_Q,DO_ASSOC,DO_THROW,DO_GET,DO_CONTAINS,DO_KEYS,DO_VALS,DO_SEQUENTIAL_Q
  DO_40_49:
  ON G-39 GOTO DO_CONS,DO_CONCAT,DO_NTH,DO_FIRST,DO_REST,DO_EMPTY_Q,DO_COUNT,DO_CONJ,DO_SEQ,DO_WITH_META
  DO_50_59:
  ON G-49 GOTO DO_META,DO_ATOM,DO_ATOM_Q,DO_DEREF,DO_RESET_BANG,DO_EVAL,DO_READ_FILE,DO_PR_MEMORY_SUMMARY

  DO_EQUAL_Q:
    GOSUB EQUAL_Q
    GOTO RETURN_TRUE_FALSE
  DO_THROW:
    ER=A
    Z%(ER)=Z%(ER)+32
    R=-1
    RETURN
  DO_NIL_Q:
    R=A=0
    GOTO RETURN_TRUE_FALSE
  DO_TRUE_Q:
    R=A=4
    GOTO RETURN_TRUE_FALSE
  DO_FALSE_Q:
    R=A=2
    GOTO RETURN_TRUE_FALSE
  DO_STRING_Q:
    R=0
    IF (Z%(A)AND 31)<>4 THEN GOTO RETURN_TRUE_FALSE
    IF MID$(S$(Z%(A+1)),1,1)=CHR$(127) THEN GOTO RETURN_TRUE_FALSE
    R=1
    GOTO RETURN_TRUE_FALSE
  DO_SYMBOL:
    B$=S$(Z%(A+1))
    T=5:GOSUB STRING
    RETURN
  DO_SYMBOL_Q:
    R=(Z%(A)AND 31)=5
    GOTO RETURN_TRUE_FALSE
  DO_KEYWORD:
    B$=S$(Z%(A+1))
    IF MID$(B$,1,1)<>CHR$(127) THEN B$=CHR$(127)+B$
    T=4:GOSUB STRING
    RETURN
  DO_KEYWORD_Q:
    R=0
    IF (Z%(A)AND 31)<>4 THEN GOTO RETURN_TRUE_FALSE
    IF MID$(S$(Z%(A+1)),1,1)<>CHR$(127) THEN GOTO RETURN_TRUE_FALSE
    R=1
    GOTO RETURN_TRUE_FALSE

  DO_PR_STR:
    AZ=AR:B=1:B$=" ":GOSUB PR_STR_SEQ
    B$=R$:T=4:GOSUB STRING
    RETURN
  DO_STR:
    AZ=AR:B=0:B$="":GOSUB PR_STR_SEQ
    B$=R$:T=4:GOSUB STRING
    RETURN
  DO_PRN:
    AZ=AR:B=1:B$=" ":GOSUB PR_STR_SEQ
    PRINT R$
    R=0
    GOTO RETURN_INC_REF
  DO_PRINTLN:
    AZ=AR:B=0:B$=" ":GOSUB PR_STR_SEQ
    PRINT R$
    R=0
    GOTO RETURN_INC_REF
  DO_READ_STRING:
    A$=S$(Z%(A+1))
    GOSUB READ_STR
    RETURN
  DO_READLINE:
    A$=S$(Z%(A+1)):GOSUB READLINE
    IF EZ=1 THEN EZ=0:R=0:GOTO RETURN_INC_REF
    B$=R$:T=4:GOSUB STRING
    RETURN
  DO_SLURP:
    R$=""
    #cbm OPEN 1,8,0,S$(Z%(A+1))
    #qbasic A$=S$(Z%(A+1))
    #qbasic IF NOT _FILEEXISTS(A$) THEN ER=-1:E$="File not found":RETURN
    #qbasic OPEN A$ FOR INPUT AS #1
    DO_SLURP_LOOP:
      A$=""
      #cbm GET#1,A$
      #qbasic A$=INPUT$(1,1)
      #qbasic IF EOF(1) THEN EZ=1:A$=A$+CHR$(10)+")":GOTO DO_SLURP_DONE
      IF ASC(A$)=10 THEN R$=R$+CHR$(13)
      IF (ASC(A$)<>10) AND (A$<>"") THEN R$=R$+A$
      #cbm IF (ST AND 64) THEN GOTO DO_SLURP_DONE
      #cbm IF (ST AND 255) THEN ER=-1:E$="File read error "+STR$(ST):RETURN
      GOTO DO_SLURP_LOOP
    DO_SLURP_DONE:
      CLOSE 1
      B$=R$:T=4:GOSUB STRING
      RETURN

  DO_LT:
    R=Z%(A+1)<Z%(B+1)
    GOTO RETURN_TRUE_FALSE
  DO_LTE:
    R=Z%(A+1)<=Z%(B+1)
    GOTO RETURN_TRUE_FALSE
  DO_GT:
    R=Z%(A+1)>Z%(B+1)
    GOTO RETURN_TRUE_FALSE
  DO_GTE:
    R=Z%(A+1)>=Z%(B+1)
    GOTO RETURN_TRUE_FALSE

  DO_ADD:
    T=2:L=Z%(A+1)+Z%(B+1):GOSUB ALLOC
    RETURN
  DO_SUB:
    T=2:L=Z%(A+1)-Z%(B+1):GOSUB ALLOC
    RETURN
  DO_MULT:
    T=2:L=Z%(A+1)*Z%(B+1):GOSUB ALLOC
    RETURN
  DO_DIV:
    T=2:L=Z%(A+1)/Z%(B+1):GOSUB ALLOC
    RETURN
  DO_TIME_MS:
    T=2:L=INT((TI-BT)*16.667):GOSUB ALLOC
    RETURN

  DO_LIST:
    R=AR
    GOTO RETURN_INC_REF
  DO_LIST_Q:
    GOSUB LIST_Q
    GOTO RETURN_TRUE_FALSE
  DO_VECTOR:
    A=AR:T=7:GOSUB FORCE_SEQ_TYPE
    RETURN
  DO_VECTOR_Q:
    R=(Z%(A)AND 31)=7
    GOTO RETURN_TRUE_FALSE
  DO_HASH_MAP:
    REM setup the stack for the loop
    T=8:GOSUB MAP_LOOP_START

    A=AR
    DO_HASH_MAP_LOOP:
      IF Z%(A+1)=0 THEN GOTO DO_HASH_MAP_LOOP_DONE

      M=Z%(A+2)
      N=Z%(Z%(A+1)+2)

      A=Z%(Z%(A+1)+1): REM skip two

      REM update the return sequence structure
      REM do not release M and N since we are pulling them from the
      REM arguments (and not creating them here)
      C=0:GOSUB MAP_LOOP_UPDATE

      GOTO DO_HASH_MAP_LOOP

    DO_HASH_MAP_LOOP_DONE:
      REM cleanup stack and get return value
      GOSUB MAP_LOOP_DONE
      RETURN

  DO_MAP_Q:
    R=(Z%(A)AND 31)=8
    GOTO RETURN_TRUE_FALSE
  DO_ASSOC:
    H=A
    AR=Z%(AR+1)
    DO_ASSOC_LOOP:
      K=Z%(AR+2)
      C=Z%(Z%(AR+1)+2)
      Z%(H)=Z%(H)+32
      GOSUB ASSOC1:H=R
      AR=Z%(Z%(AR+1)+1)
      IF AR=0 OR Z%(AR+1)=0 THEN RETURN
      GOTO DO_ASSOC_LOOP
  DO_GET:
    IF A=0 THEN R=0:GOTO RETURN_INC_REF
    H=A:K=B:GOSUB HASHMAP_GET
    GOTO RETURN_INC_REF
  DO_CONTAINS:
    H=A:K=B:GOSUB HASHMAP_CONTAINS
    GOTO RETURN_TRUE_FALSE
  DO_KEYS:
    T1=0
    GOTO DO_KEYS_VALS
  DO_VALS:
    T1=1
  DO_KEYS_VALS:
    REM setup the stack for the loop
    T=6:GOSUB MAP_LOOP_START

    DO_KEYS_VALS_LOOP:
      IF Z%(A+1)=0 THEN GOTO DO_KEYS_VALS_LOOP_DONE

      IF T1=0 THEN M=Z%(A+2)
      IF T1=1 THEN M=Z%(A+3)

      A=Z%(A+1): REM next element

      REM update the return sequence structure
      REM do not release N since we are pulling it from the
      REM hash-map (and not creating them here)
      C=0:GOSUB MAP_LOOP_UPDATE

      GOTO DO_KEYS_VALS_LOOP

    DO_KEYS_VALS_LOOP_DONE:
      REM cleanup stack and get return value
      GOSUB MAP_LOOP_DONE
      RETURN

  DO_SEQUENTIAL_Q:
    R=(Z%(A)AND 31)=6 OR (Z%(A)AND 31)=7
    GOTO RETURN_TRUE_FALSE
  DO_CONS:
    T=6:L=B:M=A:GOSUB ALLOC
    RETURN
  DO_CONCAT:
    REM if empty arguments, return empty list
    IF Z%(AR+1)=0 THEN R=6:GOTO RETURN_INC_REF

    REM single argument
    IF Z%(Z%(AR+1)+1)<>0 THEN GOTO DO_CONCAT_MULT
      REM force to list type
      T=6:GOSUB FORCE_SEQ_TYPE
      RETURN

    REM multiple arguments
    DO_CONCAT_MULT:
      REM TODO: something other than direct X access?
      CZ=X: REM save current stack position
      REM push arguments onto the stack
      DO_CONCAT_STACK:
        R=Z%(AR+2)
        GOSUB PUSH_R: REM push sequence
        AR=Z%(AR+1)
        IF Z%(AR+1)<>0 THEN GOTO DO_CONCAT_STACK

    REM pop last argument as our seq to prepend to
    GOSUB POP_Q:B=Q
    REM last arg/seq is not copied so we need to inc ref to it
    Z%(B)=Z%(B)+32
    DO_CONCAT_LOOP:
      IF X=CZ THEN R=B:RETURN
      GOSUB POP_A: REM pop off next seq to prepend
      IF Z%(A+1)=0 THEN GOTO DO_CONCAT_LOOP: REM skip empty seqs
      Q=B:GOSUB PUSH_Q
      B=0:C=-1:GOSUB SLICE
      GOSUB POP_Q:B=Q

      REM release the terminator of new list (we skip over it)
      REM we already checked for an empty list above, so R6 is pointer
      REM a real non-empty list
      AY=Z%(R6+1):GOSUB RELEASE
      REM attach new list element before terminator (last actual
      REM element to the next sequence
      Z%(R6+1)=B

      B=R
      GOTO DO_CONCAT_LOOP
  DO_NTH:
    GOSUB COUNT
    B=Z%(B+1)
    IF R<=B THEN R=-1:ER=-1:E$="nth: index out of range":RETURN
    DO_NTH_LOOP:
      IF B=0 THEN GOTO DO_NTH_DONE
      B=B-1
      A=Z%(A+1)
      GOTO DO_NTH_LOOP
    DO_NTH_DONE:
      R=Z%(A+2)
      GOTO RETURN_INC_REF
  DO_FIRST:
    R=0
    IF A=0 THEN GOTO RETURN_INC_REF
    IF Z%(A+1)<>0 THEN R=Z%(A+2)
    GOTO RETURN_INC_REF
  DO_REST:
    IF A=0 THEN R=6:GOTO RETURN_INC_REF
    IF Z%(A+1)<>0 THEN A=Z%(A+1): REM get the next sequence element
    T=6:GOSUB FORCE_SEQ_TYPE
    RETURN
  DO_EMPTY_Q:
    R=Z%(A+1)=0
    GOTO RETURN_TRUE_FALSE
  DO_COUNT:
    GOSUB COUNT
    T=2:L=R:GOSUB ALLOC
    RETURN
  DO_CONJ:
    R=0
    GOTO RETURN_INC_REF
  DO_SEQ:
    R=0
    GOTO RETURN_INC_REF

  DO_WITH_META:
    T=Z%(A)AND 31
    REM remove existing metadata first
    IF T=14 THEN A=Z%(A+1):GOTO DO_WITH_META
    T=14:L=A:M=B:GOSUB ALLOC
    RETURN
  DO_META:
    R=0
    IF (Z%(A)AND 31)=14 THEN R=Z%(A+2)
    GOTO RETURN_INC_REF
  DO_ATOM:
    T=12:L=A:GOSUB ALLOC
    RETURN
  DO_ATOM_Q:
    R=(Z%(A)AND 31)=12
    GOTO RETURN_TRUE_FALSE
  DO_DEREF:
    R=Z%(A+1)
    GOTO RETURN_INC_REF
  DO_RESET_BANG:
    R=B
    REM release current value
    AY=Z%(A+1):GOSUB RELEASE
    REM inc ref by 2 for atom ownership and since we are returning it
    Z%(R)=Z%(R)+64
    REM update value
    Z%(A+1)=R
    RETURN

  REM DO_PR_MEMORY:
  REM   P1=ZT:P2=-1:GOSUB PR_MEMORY
  REM   RETURN
  DO_PR_MEMORY_SUMMARY:
    REM GOSUB PR_MEMORY_SUMMARY
    GOSUB PR_MEMORY_SUMMARY_SMALL
    R=0
    GOTO RETURN_INC_REF
    RETURN

  DO_EVAL:
    Q=E:GOSUB PUSH_Q: REM push/save environment
    E=D:CALL EVAL
    GOSUB POP_Q:E=Q
    RETURN

  DO_READ_FILE:
    A$=S$(Z%(A+1))
    GOSUB READ_FILE
    RETURN

INIT_CORE_SET_FUNCTION:
  GOSUB NATIVE_FUNCTION
  C=R:GOSUB ENV_SET_S
  A=A+1
  RETURN

REM INIT_CORE_NS(E)
INIT_CORE_NS:
  REM create the environment mapping
  REM must match DO_FUNCTION mappings

  A=1
  B$="=":GOSUB INIT_CORE_SET_FUNCTION: REM A=1
  B$="throw":GOSUB INIT_CORE_SET_FUNCTION: REM A=2
  B$="nil?":GOSUB INIT_CORE_SET_FUNCTION: REM A=3
  B$="true?":GOSUB INIT_CORE_SET_FUNCTION: REM A=4
  B$="false?":GOSUB INIT_CORE_SET_FUNCTION: REM A=5
  B$="string?":GOSUB INIT_CORE_SET_FUNCTION: REM A=6
  B$="symbol":GOSUB INIT_CORE_SET_FUNCTION: REM A=7
  B$="symbol?":GOSUB INIT_CORE_SET_FUNCTION: REM A=8
  B$="keyword":GOSUB INIT_CORE_SET_FUNCTION: REM A=9
  B$="keyword?":GOSUB INIT_CORE_SET_FUNCTION: REM A=10

  B$="pr-str":GOSUB INIT_CORE_SET_FUNCTION: REM A=11
  B$="str":GOSUB INIT_CORE_SET_FUNCTION: REM A=12
  B$="prn":GOSUB INIT_CORE_SET_FUNCTION: REM A=13
  B$="println":GOSUB INIT_CORE_SET_FUNCTION: REM A=14
  B$="read-string":GOSUB INIT_CORE_SET_FUNCTION: REM A=15
  B$="readline":GOSUB INIT_CORE_SET_FUNCTION: REM A=16
  B$="slurp":GOSUB INIT_CORE_SET_FUNCTION: REM A=17

  B$="<":GOSUB INIT_CORE_SET_FUNCTION: REM A=18
  B$="<=":GOSUB INIT_CORE_SET_FUNCTION: REM A=19
  B$=">":GOSUB INIT_CORE_SET_FUNCTION: REM A=20
  B$=">=":GOSUB INIT_CORE_SET_FUNCTION: REM A=21
  B$="+":GOSUB INIT_CORE_SET_FUNCTION: REM A=22
  B$="-":GOSUB INIT_CORE_SET_FUNCTION: REM A=23
  B$="*":GOSUB INIT_CORE_SET_FUNCTION: REM A=24
  B$="/":GOSUB INIT_CORE_SET_FUNCTION: REM A=25
  B$="time-ms":GOSUB INIT_CORE_SET_FUNCTION: REM A=26

  B$="list":GOSUB INIT_CORE_SET_FUNCTION: REM A=27
  B$="list?":GOSUB INIT_CORE_SET_FUNCTION: REM A=28
  B$="vector":GOSUB INIT_CORE_SET_FUNCTION: REM A=29
  B$="vector?":GOSUB INIT_CORE_SET_FUNCTION: REM A=30
  B$="hash-map":GOSUB INIT_CORE_SET_FUNCTION: REM A=31
  B$="map?":GOSUB INIT_CORE_SET_FUNCTION: REM A=32
  B$="assoc":GOSUB INIT_CORE_SET_FUNCTION: REM A=33
  B$="dissoc":GOSUB INIT_CORE_SET_FUNCTION: REM A=34
  B$="get":GOSUB INIT_CORE_SET_FUNCTION: REM A=35
  B$="contains?":GOSUB INIT_CORE_SET_FUNCTION: REM A=36
  B$="keys":GOSUB INIT_CORE_SET_FUNCTION: REM A=37
  B$="vals":GOSUB INIT_CORE_SET_FUNCTION: REM A=38

  B$="sequential?":GOSUB INIT_CORE_SET_FUNCTION: REM A=39
  B$="cons":GOSUB INIT_CORE_SET_FUNCTION: REM A=40
  B$="concat":GOSUB INIT_CORE_SET_FUNCTION: REM A=41
  B$="nth":GOSUB INIT_CORE_SET_FUNCTION: REM A=42
  B$="first":GOSUB INIT_CORE_SET_FUNCTION: REM A=43
  B$="rest":GOSUB INIT_CORE_SET_FUNCTION: REM A=44
  B$="empty?":GOSUB INIT_CORE_SET_FUNCTION: REM A=45
  B$="count":GOSUB INIT_CORE_SET_FUNCTION: REM A=46

  B$="conj":GOSUB INIT_CORE_SET_FUNCTION: REM A=47
  B$="seq":GOSUB INIT_CORE_SET_FUNCTION: REM A=48

  B$="with-meta":GOSUB INIT_CORE_SET_FUNCTION: REM A=49
  B$="meta":GOSUB INIT_CORE_SET_FUNCTION: REM A=50
  B$="atom":GOSUB INIT_CORE_SET_FUNCTION: REM A=51
  B$="atom?":GOSUB INIT_CORE_SET_FUNCTION: REM A=52
  B$="deref":GOSUB INIT_CORE_SET_FUNCTION: REM A=53
  B$="reset!":GOSUB INIT_CORE_SET_FUNCTION: REM A=54

  B$="eval":GOSUB INIT_CORE_SET_FUNCTION: REM A=55
  B$="read-file":GOSUB INIT_CORE_SET_FUNCTION: REM A=56
  B$="pr-memory-summary":GOSUB INIT_CORE_SET_FUNCTION: REM A=57

  REM these are in DO_TCO_FUNCTION
  A=61
  B$="apply":GOSUB INIT_CORE_SET_FUNCTION: REM A=61
  B$="map":GOSUB INIT_CORE_SET_FUNCTION: REM A=62
  B$="swap!":GOSUB INIT_CORE_SET_FUNCTION: REM A=63

  RETURN
