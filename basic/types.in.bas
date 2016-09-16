REM                  Z 0   ->  1
REM nil                0   ->  (unused)
REM boolean            1   ->  0: false, 1: true
REM integer            2   ->  int value
REM float              3   ->  ???
REM string/kw          4   ->  ZS$ index
REM symbol             5   ->  ZS$ index
REM list next/val      6   ->  next Z% index (0 for last)
REM                    followed by value (unless empty)
REM vector next/val    7   ->  next Z% index (0 for last)
REM                    followed by value (unless empty)
REM hashmap next/val   8   ->  next Z% index (0 for last)
REM                    followed by key or value (alternating)
REM function           9   ->  function index
REM mal function       10  ->  body AST Z% index
REM                    followed by param and env Z% index
REM atom               11  ->  Z% index
REM environment        13  ->  data/hashmap Z% index
REM                    followed by 13 and outer Z% index (-1 for none)
REM reference/ptr      14  ->  Z% index / or 0
REM next free ptr      15  ->  Z% index / or 0

INIT_MEMORY:
  T%=FRE(0)

  S1%=3072: REM Z% (boxed memory) size (X2)
  REM S1%=4096: REM Z% (boxed memory) size (X2)
  S2%=256: REM ZS% (string memory) size
  S3%=256: REM ZZ% (call stack) size
  S4%=64: REM ZR% (pending release stack) size

  REM global error state
  ER%=0
  ER$=""

  REM boxed element memory
  DIM Z%(S1%,1): REM TYPE ARRAY

  REM Predefine nil, false, true
  Z%(0,0) = 0
  Z%(0,1) = 0
  Z%(1,0) = 1
  Z%(1,1) = 0
  Z%(2,0) = 1
  Z%(2,1) = 1

  REM start of unused memory
  ZI%=3

  REM start of free list
  ZK%=3

  REM string memory storage
  ZJ%=0
  DIM ZS$(S2%)

  REM call/logic stack
  ZL%=-1
  DIM ZZ%(S3%): REM stack of Z% indexes

  REM pending release stack
  ZM%=-1
  DIM ZR%(S4%): REM stack of Z% indexes

  REM PRINT "Lisp data memory: " + STR$(T%-FRE(0))
  REM PRINT "Interpreter working memory: " + STR$(FRE(0))
  RETURN

REM memory functions

REM ALLOC(SZ%) -> R%
ALLOC:
  REM PRINT "ALLOC SZ%: "+STR$(SZ%)+", ZK%: "+STR$(ZK%)
  U3%=ZK%
  U4%=ZK%
  ALLOC_LOOP:
    IF U4%=ZI% THEN GOTO ALLOC_UNUSED
    REM TODO sanity check that type is 15
    IF ((Z%(U4%,0)AND-16)/16)=SZ% THEN GOTO ALLOC_MIDDLE
    REM PRINT "ALLOC search: U3%: "+STR$(U3%)+", U4%: "+STR$(U4%)
    U3%=U4%: REM previous set to current
    U4%=Z%(U4%,1): REM current set to next
    GOTO ALLOC_LOOP
  ALLOC_MIDDLE:
    REM PRINT "ALLOC_MIDDLE: U3%: "+STR$(U3%)+", U4%: "+STR$(U4%)
    R%=U4%
    REM set free pointer (ZK%) to next free
    IF U4%=ZK% THEN ZK%=Z%(U4%,1)
    REM set previous free to next free
    IF U4%<>ZK% THEN Z%(U3%,1)=Z%(U4%,1)
    RETURN
  ALLOC_UNUSED:
    REM PRINT "ALLOC_UNUSED ZI%: "+STR$(ZI%)+", U3%: "+STR$(U3%)+", U4%: "+STR$(U4%)
    R%=U4%
    ZI%=ZI%+SZ%
    IF U3%=U4% THEN ZK%=ZI%
    REM set previous free to new memory top
    IF U3%<>U4% THEN Z%(U3%,1)=ZI%
    RETURN

REM FREE(AY%, SZ%) -> nil
FREE:
  REM assumes reference count cleanup already (see RELEASE)
  Z%(AY%,0) = (SZ%*16)+15: REM set type(15) and size
  Z%(AY%,1) = ZK%
  IF SZ%>=2 THEN Z%(AY%+1,0)=0
  IF SZ%>=2 THEN Z%(AY%+1,1)=0
  IF SZ%>=3 THEN Z%(AY%+2,0)=0
  IF SZ%>=3 THEN Z%(AY%+2,1)=0
  ZK%=AY%
  RETURN


REM RELEASE(AY%) -> nil
RELEASE:
  RC%=0

  GOTO RELEASE_ONE

  RELEASE_TOP:

  IF RC%=0 THEN RETURN

  REM pop next object to release, decrease remaining count
  AY%=ZZ%(ZL%): ZL%=ZL%-1
  RC%=RC%-1

  RELEASE_ONE:

  REM nil, false, true
  IF AY%<3 THEN GOTO RELEASE_TOP

  REM sanity check not already freed
  IF (Z%(AY%,0)AND15)=15 THEN ER%=1: ER$="Free of free mem: " + STR$(AY%): RETURN
  IF Z%(AY%,0)<16 THEN ER%=1: ER$="Free of freed object: " + STR$(AY%): RETURN

  REM decrease reference count by one
  Z%(AY%,0)=Z%(AY%,0)-16

  REM our reference count is not 0, so don't release
  IF Z%(AY%,0)>=16 GOTO RELEASE_TOP

  REM switch on type
  U6%=Z%(AY%,0)AND15: REM type
  IF (U6%<=5) OR (U6%=9) THEN GOTO RELEASE_SIMPLE
  IF (U6%>=6) AND (U6%<=8) THEN GOTO RELEASE_SEQ
  IF U6%=10 THEN GOTO RELEASE_MAL_FUNCTION
  IF U6%=13 THEN GOTO RELEASE_ENV
  IF U6%=14 THEN GOTO RELEASE_REFERENCE
  IF U6%=15 THEN ER%=1: ER$="RELEASE of already freed: "+STR$(AY%): RETURN
  ER%=1: ER$="RELEASE not defined for type " + STR$(U6%): RETURN

  RELEASE_SIMPLE:
    REM simple type (no recursing), just call FREE on it
    SZ%=1: GOSUB FREE
    GOTO RELEASE_TOP
  RELEASE_SIMPLE_2:
    REM free the current element and continue
    SZ%=2: GOSUB FREE
    GOTO RELEASE_TOP
  RELEASE_SEQ:
    IF Z%(AY%,1)=0 THEN GOTO RELEASE_SIMPLE_2
    IF Z%(AY%+1,0)<>14 THEN ER%=1: ER$="invalid list value"+STR$(AY%+1): RETURN
    REM add value and next element to stack
    RC%=RC%+2: ZL%=ZL%+2: ZZ%(ZL%-1)=Z%(AY%+1,1): ZZ%(ZL%)=Z%(AY%,1)
    GOTO RELEASE_SIMPLE_2
  RELEASE_MAL_FUNCTION:
    REM add ast, params and environment to stack
    RC%=RC%+3: ZL%=ZL%+3
    ZZ%(ZL%-2)=Z%(AY%,1): ZZ%(ZL%-1)=Z%(AY%+1,0): ZZ%(ZL%)=Z%(AY%+1,1)
    REM free the current 2 element mal_function and continue
    SZ%=2: GOSUB FREE
    GOTO RELEASE_TOP
  RELEASE_ENV:
    REM add the hashmap data to the stack
    RC%=RC%+1: ZL%=ZL%+1: ZZ%(ZL%)=Z%(AY%,1)
    REM if no outer set
    IF Z%(AY%+1,1)=-1 THEN GOTO RELEASE_ENV_FREE
    REM add outer environment to the stack
    RC%=RC%+1: ZL%=ZL%+1: ZZ%(ZL%)=Z%(AY%+1,1)
    RELEASE_ENV_FREE:
      REM free the current 2 element environment and continue
      SZ%=2: GOSUB FREE
      GOTO RELEASE_TOP
  RELEASE_REFERENCE:
    IF Z%(AY%,1)=0 THEN GOTO RELEASE_SIMPLE
    REM add the referred element to the stack
    RC%=RC%+1: ZL%=ZL%+1: ZZ%(ZL%)=Z%(AY%,1)
    REM free the current element and continue
    SZ%=1: GOSUB FREE
    GOTO RELEASE_TOP

REM RELEASE_PEND() -> nil
RELEASE_PEND:
  IF ZM%<0 THEN RETURN
  AY%=ZR%(ZM%): GOSUB RELEASE
  ZM%=ZM%-1
  GOTO RELEASE_PEND

REM DEREF_R(R%) -> R%
DEREF_R:
  IF (Z%(R%,0)AND15)=14 THEN R%=Z%(R%,1): GOTO DEREF_R
  RETURN

REM DEREF_A(A%) -> A%
DEREF_A:
  IF (Z%(A%,0)AND15)=14 THEN A%=Z%(A%,1): GOTO DEREF_A
  RETURN

REM DEREF_B(B%) -> B%
DEREF_B:
  IF (Z%(B%,0)AND15)=14 THEN B%=Z%(B%,1): GOTO DEREF_B
  RETURN

CHECK_FREE_LIST:
  P1%=ZK%: P2%=0: REM start and accumulator
  CHECK_FREE_LIST_LOOP:
    IF P1%>=ZI% THEN GOTO CHECK_FREE_LIST_DONE
    IF (Z%(P1%,0)AND15)<>15 THEN P2%=-1: GOTO CHECK_FREE_LIST_DONE
    P2%=P2%+(Z%(P1%,0)AND-16)/16
    P1%=Z%(P1%,1)
    GOTO CHECK_FREE_LIST_LOOP
  CHECK_FREE_LIST_DONE:
    IF P2%=-1 THEN PRINT "corrupt free list at "+STR$(P1%)
    RETURN

PR_MEMORY_SUMMARY:
  GOSUB CHECK_FREE_LIST: REM get count in P2%
  PRINT
  PRINT "Free memory (FRE)      : " + STR$(FRE(0))
  PRINT "Value memory (Z%)      : " + STR$(ZI%-1) + " /" + STR$(S1%)
  PRINT "                         ";
  PRINT " used:"+STR$(ZI%-1-P2%)+", freed:"+STR$(P2%);
  PRINT ", post repl_env:"+STR$(ZT%)
  PRINT "String values (ZS$)    : " + STR$(ZJ%) + " /" + STR$(S2%)
  PRINT "Call stack size (ZZ%)  : " + STR$(ZL%+1) + " /" + STR$(S3%)
  RETURN

REM PR_MEMORY(P1%, P2%) -> nil
PR_MEMORY:
  IF P2%<P1% THEN P2%=ZI%-1
  PRINT "vvvvvv"
  PRINT "Z% Value Memory"+STR$(P1%)+"->"+STR$(P2%);
  PRINT " (ZI%: "+STR$(ZI%)+", ZK%: "+STR$(ZK%)+"):"
  IF P2%<P1% THEN PRINT "  ---": GOTO PR_MEMORY_AFTER_VALUES
  I=P1%
  PR_MEMORY_VALUE_LOOP:
    IF I>P2% THEN GOTO PR_MEMORY_AFTER_VALUES
    PRINT " " + STR$(I);
    IF (Z%(I,0)AND15)=15 THEN GOTO PR_MEMORY_FREE
      PRINT ": ref cnt: " + STR$((Z%(I,0)AND-16)/16);
      PRINT ", type: " + STR$(Z%(I,0)AND15) + ", value: " + STR$(Z%(I,1))
      I=I+1
      GOTO PR_MEMORY_VALUE_LOOP
    PR_MEMORY_FREE:
      PRINT ": FREE size: "+STR$((Z%(I,0)AND-16)/16)+", next: "+STR$(Z%(I,1));
      IF I=ZK% THEN PRINT " (free list start)";
      PRINT
      IF (Z%(I,0)AND-16)=32 THEN I=I+1: PRINT " " + STR$(I) + ": ---"
      I=I+1
      GOTO PR_MEMORY_VALUE_LOOP
  PR_MEMORY_AFTER_VALUES:
  PRINT "ZS% String Memory (ZJ%: " + STR$(ZJ%) + "):"
  IF ZJ%<=0 THEN PRINT "  ---": GOTO PR_MEMORY_SKIP_STRINGS
  FOR I=0 TO ZJ%-1
    PRINT " " + STR$(I) + ": '" + ZS$(I) + "'"
    NEXT I
  PR_MEMORY_SKIP_STRINGS:
  PRINT "ZZ% Stack Memory (ZL%: " + STR$(ZL%) + "):"
  IF ZL%<0 THEN PRINT "  ---": GOTO PR_MEMORY_SKIP_STACK
  FOR I=0 TO ZL%
    PRINT " "+STR$(I)+": "+STR$(ZZ%(I))
    NEXT I
  PR_MEMORY_SKIP_STACK:
  PRINT "^^^^^^"
  RETURN


REM general functions

REM EQUAL_Q(A%, B%) -> R%
EQUAL_Q:
  GOSUB DEREF_A: GOSUB DEREF_B

  R%=0
  U1%=(Z%(A%,0)AND15): U2%=(Z%(B%,0)AND15)
  IF NOT ((U1%=U2%) OR ((U1%=6 OR U1%=7) AND (U2%=6 OR U2%=7))) THEN RETURN
  IF U1%=6 THEN GOTO EQUAL_Q_SEQ
  IF U1%=7 THEN GOTO EQUAL_Q_SEQ
  IF U1%=8 THEN GOTO EQUAL_Q_HM

  IF Z%(A%,1)=Z%(B%,1) THEN R%=1
  RETURN

  EQUAL_Q_SEQ:
    IF (Z%(A%,1)=0) AND (Z%(B%,1)=0) THEN R%=1: RETURN
    IF (Z%(A%,1)=0) OR (Z%(B%,1)=0) THEN R%=0: RETURN

    REM push A% and B%
    ZL%=ZL%+2: ZZ%(ZL%-1)=A%: ZZ%(ZL%)=B%
    A%=Z%(A%+1,1): B%=Z%(B%+1,1): GOSUB EQUAL_Q
    REM pop A% and B%
    A%=ZZ%(ZL%-1): B%=ZZ%(ZL%): ZL%=ZL%-2
    IF R%=0 THEN RETURN

    REM next elements of the sequences
    A%=Z%(A%,1): B%=Z%(B%,1): GOTO EQUAL_Q_SEQ
  EQUAL_Q_HM:
    R%=0
    RETURN

REM string functions

REM STRING_(AS$) -> R%
REM intern string (returns string index, not Z% index)
STRING:
  IF ZJ%=0 THEN GOTO STRING_NOT_FOUND

  REM search for matching string in ZS$
  FOR I=0 TO ZJ%-1
    IF AS$=ZS$(I) THEN R%=I: RETURN
    NEXT I

  STRING_NOT_FOUND:
    ZS$(ZJ%) = AS$
    R%=ZJ%
    ZJ%=ZJ%+1
    RETURN




REM list functions

REM LIST_Q(A%) -> R%
LIST_Q:
  R%=0
  IF (Z%(A%,0)AND15)=6 THEN R%=1
  RETURN

REM EMPTY_Q(A%) -> R%
EMPTY_Q:
  R%=0
  IF Z%(A%,1)=0 THEN R%=1
  RETURN

REM COUNT(A%) -> R%
COUNT:
  R%=-1
  DO_COUNT_LOOP:
    R%=R%+1
    IF Z%(A%,1)<>0 THEN A%=Z%(A%,1): GOTO DO_COUNT_LOOP
  RETURN

REM LAST(A%) -> R%
LAST:
  REM TODO check that actually a list/vector
  IF Z%(A%,1)=0 THEN R%=0: RETURN: REM empty seq, return nil
  T6%=0
  LAST_LOOP:
    IF Z%(A%,1)=0 THEN GOTO LAST_DONE: REM end, return previous value
    T6%=A%: REM current becomes previous entry
    A%=Z%(A%,1): REM next entry
    GOTO LAST_LOOP
  LAST_DONE:
    R%=T6%+1: GOSUB DEREF_R
    Z%(R%,0)=Z%(R%,0)+16
    RETURN

REM hashmap functions

REM HASHMAP() -> R%
HASHMAP:
  SZ%=2: GOSUB ALLOC
  Z%(R%,0) = 8+16
  Z%(R%,1) = 0
  Z%(R%+1,0) = 14
  Z%(R%+1,1) = 0
  RETURN

REM ASSOC1(HM%, K%, V%) -> R%
ASSOC1:
  REM deref to actual key and value
  R%=K%: GOSUB DEREF_R: K%=R%
  R%=V%: GOSUB DEREF_R: V%=R%

  REM inc ref count of key and value
  Z%(K%,0)=Z%(K%,0)+16
  Z%(V%,0)=Z%(V%,0)+16
  SZ%=4: GOSUB ALLOC
  REM key ptr
  Z%(R%,0) = 8+16
  Z%(R%,1) = R%+2: REM point to next element (value)
  Z%(R%+1,0) = 14
  Z%(R%+1,1) = K%
  REM value ptr
  Z%(R%+2,0) = 8+16
  Z%(R%+2,1) = HM%: REM hashmap to assoc onto
  Z%(R%+3,0) = 14
  Z%(R%+3,1) = V%
  RETURN

REM ASSOC1(HM%, K$, V%) -> R%
ASSOC1_S:
  REM add the key string, then call ASSOC1
  SZ%=1: GOSUB ALLOC
  K%=R%
  ZS$(ZJ%) = K$
  Z%(R%,0) = 4: REM key ref cnt will be inc'd by ASSOC1
  Z%(R%,1) = ZJ%
  ZJ%=ZJ%+1
  GOSUB ASSOC1
  RETURN

REM HASHMAP_GET(HM%, K%) -> R%
HASHMAP_GET:
  H2%=HM%
  T1$=ZS$(Z%(K%,1)): REM search key string
  T3%=0: REM whether found or not (for HASHMAP_CONTAINS)
  R%=0
  HASHMAP_GET_LOOP:
    REM no matching key found
    IF Z%(H2%,1)=0 THEN R%=0: RETURN
    REM follow value ptrs
    T2%=H2%+1
    HASHMAP_GET_DEREF:
      IF Z%(T2%,0)=14 THEN T2%=Z%(T2%,1): GOTO HASHMAP_GET_DEREF
    REM get key string
    T2$=ZS$(Z%(T2%,1))
    REM if they are equal, we found it
    IF T1$=T2$ THEN T3%=1: R%=Z%(H2%,1)+1: RETURN
    REM skip to next key
    H2%=Z%(Z%(H2%,1),1)
    GOTO HASHMAP_GET_LOOP

REM HASHMAP_CONTAINS(HM%, K%) -> R%
HASHMAP_CONTAINS:
  GOSUB HASHMAP_GET
  R%=T3%
  RETURN

REM NATIVE_FUNCTION(A%) -> R%
NATIVE_FUNCTION:
  SZ%=1: GOSUB ALLOC
  Z%(R%,0) = 9+16
  Z%(R%,1) = A%
  RETURN

REM NATIVE_FUNCTION(A%, P%, E%) -> R%
MAL_FUNCTION:
  SZ%=2: GOSUB ALLOC
  Z%(A%,0)=Z%(A%,0)+16
  Z%(P%,0)=Z%(P%,0)+16
  Z%(E%,0)=Z%(E%,0)+16

  Z%(R%,0) = 10+16
  Z%(R%,1) = A%
  Z%(R%+1,0) = P%
  Z%(R%+1,1) = E%
  RETURN
