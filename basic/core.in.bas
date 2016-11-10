REM APPLY should really be in types.in.bas but it is here because it
REM calls DO_TCO_FUNCTION so it will cause syntax errors for steps1-3
REM if it is in types.in.bas because there are unresolved labels.

REM APPLY(F, AR) -> R
REM   - restores E
REM   - call using GOTO and with return label/address on the stack
SUB APPLY
  REM if metadata, get the actual object
  IF (Z%(F,0)AND 31)>=16 THEN F=Z%(F,1)

  ON (Z%(F,0)AND 31)-8 GOTO APPLY_FUNCTION,APPLY_MAL_FUNCTION,APPLY_MAL_FUNCTION

  APPLY_FUNCTION:
    REM regular function
    IF Z%(F,1)<60 THEN GOSUB DO_FUNCTION:GOTO APPLY_DONE
    REM for recur functions (apply, map, swap!), use GOTO
    IF Z%(F,1)>60 THEN CALL DO_TCO_FUNCTION
    GOTO APPLY_DONE

  APPLY_MAL_FUNCTION:
    Q=E:GOSUB PUSH_Q: REM save the current environment

    REM create new environ using env and params stored in the
    REM function and bind the params to the apply arguments
    C=Z%(F+1,1):A=Z%(F+1,0):B=AR:GOSUB ENV_NEW_BINDS

    A=Z%(F,1):E=R:CALL EVAL

    AY=E:GOSUB RELEASE: REM release the new environment

    GOSUB POP_Q:E=Q: REM pop/restore the saved environment

  APPLY_DONE:
END SUB


REM DO_TCO_FUNCTION(F, AR)
SUB DO_TCO_FUNCTION
  G=Z%(F,1)

  REM Get argument values
  R=AR+1:GOSUB DEREF_R:A=R
  R=Z%(AR,1)+1:GOSUB DEREF_R:B=R

  ON G-60 GOTO DO_APPLY,DO_MAP,DO_SWAP_BANG

  DO_APPLY:
    F=A
    AR=Z%(AR,1)
    A=AR:GOSUB COUNT:C=R

    A=Z%(AR+1,1)
    REM no intermediate args, but not a list, so convert it first
    IF C<=1 AND (Z%(A,0)AND 31)<>6 THEN T=6:GOSUB FORCE_SEQ_TYPE:GOTO DO_APPLY_2
    REM no intermediate args, just call APPLY directly
    IF C<=1 THEN GOTO DO_APPLY_1

    REM prepend intermediate args to final args element
    A=AR:B=0:C=C-1:GOSUB SLICE
    REM release the terminator of new list (we skip over it)
    AY=Z%(R6,1):GOSUB RELEASE
    REM attach end of slice to final args element
    Z%(R6,1)=Z%(A+1,1)
    Z%(Z%(A+1,1),0)=Z%(Z%(A+1,1),0)+32

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

    REM first result list element
    T=6:L=0:N=0:GOSUB ALLOC

    REM push future return val, prior entry, F and B
    GOSUB PUSH_R
    Q=0:GOSUB PUSH_Q
    Q=F:GOSUB PUSH_Q
    Q=B:GOSUB PUSH_Q

    DO_MAP_LOOP:
      REM set previous to current if not the first element
      GOSUB PEEK_Q_2
      IF Q<>0 THEN Z%(Q,1)=R
      REM update previous reference to current
      Q=R:GOSUB PUT_Q_2

      IF Z%(B,1)=0 THEN GOTO DO_MAP_DONE

      REM create argument list for apply call
      Z%(3,0)=Z%(3,0)+32
      REM inc ref cnt of referred argument
      T=6:L=3:N=Z%(B+1,1):GOSUB ALLOC

      REM push argument list
      GOSUB PUSH_R

      AR=R:CALL APPLY

      REM pop apply args and release them
      GOSUB POP_Q:AY=Q
      GOSUB RELEASE

      REM set the result value
      GOSUB PEEK_Q_2
      Z%(Q+1,1)=R

      IF ER<>-2 THEN GOTO DO_MAP_DONE

      REM restore F
      GOSUB PEEK_Q_1:F=Q

      REM update B to next source element
      GOSUB PEEK_Q
      Q=Z%(Q,1)
      B=Q
      GOSUB PUT_Q

      REM allocate next element
      T=6:L=0:N=0:GOSUB ALLOC

      GOTO DO_MAP_LOOP

    DO_MAP_DONE:
      Q=3:GOSUB PEEK_Q_Q: REM get return val
      REM if no error, set the return val
      IF ER=-2 THEN R=Q
      REM otherwise, free the return value and return nil
      IF ER<>-2 THEN R=0:AY=Q:GOSUB RELEASE

      REM pop everything off stack
      GOSUB POP_Q:GOSUB POP_Q:GOSUB POP_Q:GOSUB POP_Q
      GOTO DO_TCO_FUNCTION_DONE


  DO_SWAP_BANG:
    F=B

    REM add atom to front of the args list
    T=6:L=Z%(Z%(AR,1),1):N=Z%(A,1):GOSUB ALLOC: REM cons
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


REM DO_FUNCTION(F, AR)
DO_FUNCTION:
  REM Get the function number
  G=Z%(F,1)

  REM Get argument values
  R=AR+1:GOSUB DEREF_R:A=R
  R=Z%(AR,1)+1:GOSUB DEREF_R:B=R

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
    R=R+1
    RETURN
  DO_THROW:
    ER=A
    Z%(ER,0)=Z%(ER,0)+32
    R=0
    RETURN
  DO_NIL_Q:
    R=1
    IF A=0 THEN R=2
    RETURN
  DO_TRUE_Q:
    R=1
    IF A=2 THEN R=2
    RETURN
  DO_FALSE_Q:
    R=1
    IF A=1 THEN R=2
    RETURN
  DO_STRING_Q:
    R=1
    IF (Z%(A,0)AND 31)<>4 THEN RETURN
    IF MID$(S$(Z%(A,1)),1,1)=CHR$(127) THEN RETURN
    R=2
    RETURN
  DO_SYMBOL:
    B$=S$(Z%(A,1))
    T=5:GOSUB STRING
    RETURN
  DO_SYMBOL_Q:
    R=1
    IF (Z%(A,0)AND 31)=5 THEN R=2
    RETURN
  DO_KEYWORD:
    B$=S$(Z%(A,1))
    IF MID$(B$,1,1)<>CHR$(127) THEN B$=CHR$(127)+B$
    T=4:GOSUB STRING
    RETURN
  DO_KEYWORD_Q:
    R=1
    IF (Z%(A,0)AND 31)<>4 THEN RETURN
    IF MID$(S$(Z%(A,1)),1,1)<>CHR$(127) THEN RETURN
    R=2
    RETURN

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
    RETURN
  DO_PRINTLN:
    AZ=AR:B=0:B$=" ":GOSUB PR_STR_SEQ
    PRINT R$
    R=0
    RETURN
  DO_READ_STRING:
    A$=S$(Z%(A,1))
    GOSUB READ_STR
    RETURN
  DO_READLINE:
    A$=S$(Z%(A,1)):GOSUB READLINE
    IF EZ=1 THEN EZ=0:R=0:RETURN
    B$=R$:T=4:GOSUB STRING
    RETURN
  DO_SLURP:
    R$=""
    #cbm OPEN 1,8,0,S$(Z%(A,1))
    #qbasic A$=S$(Z%(A,1))
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
    R=1
    IF Z%(A,1)<Z%(B,1) THEN R=2
    RETURN
  DO_LTE:
    R=1
    IF Z%(A,1)<=Z%(B,1) THEN R=2
    RETURN
  DO_GT:
    R=1
    IF Z%(A,1)>Z%(B,1) THEN R=2
    RETURN
  DO_GTE:
    R=1
    IF Z%(A,1)>=Z%(B,1) THEN R=2
    RETURN

  DO_ADD:
    T=2:L=Z%(A,1)+Z%(B,1):GOSUB ALLOC
    RETURN
  DO_SUB:
    T=2:L=Z%(A,1)-Z%(B,1):GOSUB ALLOC
    RETURN
  DO_MULT:
    T=2:L=Z%(A,1)*Z%(B,1):GOSUB ALLOC
    RETURN
  DO_DIV:
    T=2:L=Z%(A,1)/Z%(B,1):GOSUB ALLOC
    RETURN
  DO_TIME_MS:
    T=2:L=INT((TI-BT)*16.667):GOSUB ALLOC
    RETURN

  DO_LIST:
    R=AR
    Z%(R,0)=Z%(R,0)+32
    RETURN
  DO_LIST_Q:
    GOSUB LIST_Q
    R=R+1: REM map to mal false/true
    RETURN
  DO_VECTOR:
    A=AR:T=7:GOSUB FORCE_SEQ_TYPE
    RETURN
  DO_VECTOR_Q:
    R=1
    IF (Z%(A,0)AND 31)=7 THEN R=2
    RETURN
  DO_HASH_MAP:
    A=AR:T=8:GOSUB FORCE_SEQ_TYPE
    RETURN
  DO_MAP_Q:
    R=1
    IF (Z%(A,0)AND 31)=8 THEN R=2
    RETURN
  DO_ASSOC:
    H=A
    AR=Z%(AR,1)
    DO_ASSOC_LOOP:
      R=AR+1:GOSUB DEREF_R:K=R
      R=Z%(AR,1)+1:GOSUB DEREF_R:C=R
      Z%(H,0)=Z%(H,0)+32
      GOSUB ASSOC1:H=R
      AR=Z%(Z%(AR,1),1)
      IF AR=0 OR Z%(AR,1)=0 THEN RETURN
      GOTO DO_ASSOC_LOOP
  DO_GET:
    IF A=0 THEN R=0:RETURN
    H=A:K=B:GOSUB HASHMAP_GET
    GOSUB DEREF_R
    Z%(R,0)=Z%(R,0)+32
    RETURN
  DO_CONTAINS:
    H=A:K=B:GOSUB HASHMAP_CONTAINS
    R=R+1
    RETURN
  DO_KEYS:
    GOTO DO_KEYS_VALS
  DO_VALS:
    A=Z%(A,1)
  DO_KEYS_VALS:
    REM first result list element
    T=6:L=0:N=0:GOSUB ALLOC:T2=R

    DO_KEYS_VALS_LOOP:
      IF A=0 OR Z%(A,1)=0 THEN R=T2:RETURN

      REM copy the value
      T1=Z%(A+1,1)
      REM inc ref cnt of referred argument
      Z%(T1,0)=Z%(T1,0)+32
      Z%(R+1,1)=T1

      T1=R: REM save previous
      REM allocate next element
      T=6:L=0:N=0:GOSUB ALLOC
      REM point previous element to this one
      Z%(T1,1)=R

      IF Z%(Z%(A,1),1)=0 THEN R=T2:RETURN

      A=Z%(Z%(A,1),1)

      GOTO DO_KEYS_VALS_LOOP

  DO_SEQUENTIAL_Q:
    R=1
    IF (Z%(A,0)AND 31)=6 OR (Z%(A,0)AND 31)=7 THEN R=2
    RETURN
  DO_CONS:
    T=6:L=B:N=A:GOSUB ALLOC
    RETURN
  DO_CONCAT:
    REM if empty arguments, return empty list
    IF Z%(AR,1)=0 THEN R=3:Z%(R,0)=Z%(R,0)+32:RETURN

    REM single argument
    IF Z%(Z%(AR,1),1)<>0 THEN GOTO DO_CONCAT_MULT
      REM force to list type
      T=6:GOSUB FORCE_SEQ_TYPE
      RETURN

    REM multiple arguments
    DO_CONCAT_MULT:
      REM TODO: something other than direct X access?
      CZ=X: REM save current stack position
      REM push arguments onto the stack
      DO_CONCAT_STACK:
        R=AR+1:GOSUB DEREF_R
        GOSUB PUSH_R: REM push sequence
        AR=Z%(AR,1)
        IF Z%(AR,1)<>0 THEN GOTO DO_CONCAT_STACK

    REM pop last argument as our seq to prepend to
    GOSUB POP_Q:B=Q
    REM last arg/seq is not copied so we need to inc ref to it
    Z%(B,0)=Z%(B,0)+32
    DO_CONCAT_LOOP:
      IF X=CZ THEN R=B:RETURN
      GOSUB POP_A: REM pop off next seq to prepend
      IF Z%(A,1)=0 THEN GOTO DO_CONCAT_LOOP: REM skip empty seqs
      Q=B:GOSUB PUSH_Q
      B=0:C=-1:GOSUB SLICE
      GOSUB POP_Q:B=Q

      REM release the terminator of new list (we skip over it)
      AY=Z%(R6,1):GOSUB RELEASE
      REM attach new list element before terminator (last actual
      REM element to the next sequence
      Z%(R6,1)=B

      B=R
      GOTO DO_CONCAT_LOOP
  DO_NTH:
    GOSUB COUNT
    B=Z%(B,1)
    IF R<=B THEN R=0:ER=-1:E$="nth: index out of range":RETURN
    DO_NTH_LOOP:
      IF B=0 THEN GOTO DO_NTH_DONE
      B=B-1
      A=Z%(A,1)
      GOTO DO_NTH_LOOP
    DO_NTH_DONE:
      R=Z%(A+1,1)
      Z%(R,0)=Z%(R,0)+32
      RETURN
  DO_FIRST:
    IF A=0 THEN R=0:RETURN
    IF Z%(A,1)=0 THEN R=0
    IF Z%(A,1)<>0 THEN R=A+1:GOSUB DEREF_R
    IF R<>0 THEN Z%(R,0)=Z%(R,0)+32
    RETURN
  DO_REST:
    IF A=0 THEN R=3:Z%(R,0)=Z%(R,0)+32:RETURN
    IF Z%(A,1)<>0 THEN A=Z%(A,1)
    T=6:GOSUB FORCE_SEQ_TYPE
    RETURN
  DO_EMPTY_Q:
    R=1
    IF Z%(A,1)=0 THEN R=2
    RETURN
  DO_COUNT:
    GOSUB COUNT
    T=2:L=R:GOSUB ALLOC
    RETURN
  DO_CONJ:
    R=0
    RETURN
  DO_SEQ:
    R=0
    RETURN

  DO_WITH_META:
    T=Z%(A,0)AND 31
    REM remove existing metadata first
    IF T>=16 THEN A=Z%(A,1):GOTO DO_WITH_META
    T=T+16:L=A:N=B:GOSUB ALLOC
    RETURN
  DO_META:
    IF (Z%(A,0)AND 31)<16 THEN R=0:RETURN
    R=Z%(A+1,1)
    Z%(R,0)=Z%(R,0)+32
    RETURN
  DO_ATOM:
    T=12:L=A:GOSUB ALLOC
    RETURN
  DO_ATOM_Q:
    R=1
    IF (Z%(A,0)AND 31)=12 THEN R=2
    RETURN
  DO_DEREF:
    R=Z%(A,1):GOSUB DEREF_R
    Z%(R,0)=Z%(R,0)+32
    RETURN
  DO_RESET_BANG:
    R=B
    REM release current value
    AY=Z%(A,1):GOSUB RELEASE
    REM inc ref by 2 for atom ownership and since we are returning it
    Z%(R,0)=Z%(R,0)+64
    REM update value
    Z%(A,1)=R
    RETURN

  REM DO_PR_MEMORY:
  REM   P1=ZT:P2=-1:GOSUB PR_MEMORY
  REM   RETURN
  DO_PR_MEMORY_SUMMARY:
    GOSUB PR_MEMORY_SUMMARY
    RETURN

  DO_EVAL:
    Q=E:GOSUB PUSH_Q: REM push/save environment
    E=D:CALL EVAL
    GOSUB POP_Q:E=Q
    RETURN

  DO_READ_FILE:
    A$=S$(Z%(A,1))
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
