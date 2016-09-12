REM                  Z 0   ->  1
REM nil                0   ->  (unused)
REM boolean            1   ->  0: false, 1: true
REM integer            2   ->  int value
REM float              3   ->  ???
REM string/kw          4   ->  ZS$ index
REM symbol             5   ->  ZS$ index
REM list next/val      6   ->  next Z% index / or 0
REM                    followed by value (unless empty)
REM vector next/val    8   ->  next Z% index / or 0
REM                    followed by value (unless empty)
REM hashmap next/val   10  ->  next Z% index / or 0
REM                    followed by key or value (alternating)
REM function           12  ->  function index
REM mal function       13  ->  ???
REM atom               14  ->  Z% index
REM reference/ptr      15  ->  Z% index / or 0

INIT_MEMORY:
  T%=FRE(0)
  
  S1%=4096+512: REM Z% (boxed memory) size (X2)
  S2%=256: REM ZS% (string memory) size
  S3%=256: REM ZE%,ZO% (environments) size
  S4%=256: REM ZZ% (call stack) size
  S5%=64: REM PS% (logic stack) size

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
  ZI%=3

  REM string memory storage
  ZJ%=0
  DIM ZS$(S2%)

  REM environments
  ZK%=0
  DIM ZE%(S3%): REM data hashmap Z% index
  DIM ZO%(S3%): REM outer ZE% index (or -1)

  REM call stack
  ZL%=-1
  DIM ZZ%(S4%): REM stack of Z% indexes

  REM logic stack
  PT%=-1: REM index of top of PS% stack
  DIM PS%(S5%): REM stack of Z% indexes

  REM PRINT "Lisp data memory: " + STR$(T%-FRE(0))
  REM PRINT "Interpreter working memory: " + STR$(FRE(0))
  RETURN

REM general functions

PR_MEMORY_SUMMARY:
  PRINT
  PRINT "Free memory (FRE)      : " + STR$(FRE(0))
  PRINT "Boxed values (Z%)      : " + STR$(ZI%) + " /" + STR$(S1%)
  PRINT "String values (ZS$)    : " + STR$(ZJ%) + " /" + STR$(S2%)
  PRINT "Environments (ZE%)     : " + STR$(ZK%) + " /" + STR$(S3%)
  PRINT "Call stack size (ZZ%)  : " + STR$(ZL%+1) + " /" + STR$(S4%)
  PRINT "Logic stack size (PS%) : " + STR$(PT%+1) + " /" + STR$(S5%)
  RETURN

PR_MEMORY:
  PRINT "Value Memory (Z%):"
  FOR I=0 TO ZI%-1
    PRINT " " + STR$(I) + ": type: " + STR$(Z%(I,0)) + ", value: " + STR$(Z%(I,1))
    NEXT I
  PRINT "String Memory (ZS%):"
  FOR I=0 TO ZJ%-1
    PRINT " " + STR$(I) + ": '" + ZS$(I) + "'"
    NEXT I
  RETURN

REM DEREF(R%) -> R%
DEREF:
  IF Z%(R%,0)=15 THEN R%=Z%(R%,1): GOTO DEREF
  RETURN

REM EQUAL_Q(A%, B%) -> R%
EQUAL_Q:
  R%=0
  U1%=Z%(A%,0): U2%=Z%(B%,0)
  IF NOT ((U1%=U2%) OR ((U1%=6 OR U1%=8) AND (U2%=6 OR U2%=8))) THEN RETURN
  IF U1%=6 THEN GOTO EQUAL_Q_SEQ
  IF U1%=8 THEN GOTO EQUAL_Q_SEQ
  IF U1%=10 THEN GOTO EQUAL_Q_HM

  IF Z%(A%,1)=Z%(B%,1) THEN R%=1
  RETURN

  EQUAL_Q_SEQ:
    R%=0
    RETURN
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
  IF Z%(A%,0)=6 THEN R%=1
  RETURN

REM LIST_Q(A%) -> R%
EMPTY_Q:
  R%=0
  IF Z%(A%,1)=0 THEN R%=1
  RETURN

REM LAST(A%) -> R%
LAST:
  REM TODO check that actually a list/vector
  IF Z%(A%,1)=0 THEN R%=0: RETURN: REM empty seq, return nil
  T6%=0
  LAST_LOOP:
    IF Z%(A%,1)=0 THEN R%=T6%+1: RETURN: REM end, return previous value
    T6%=A%: REM current becomes previous entry
    A%=Z%(A%,1): REM next entry
    GOTO LAST_LOOP

REM hashmap functions

REM HASHMAP() -> R%
HASHMAP:
  Z%(ZI%,0) = 10
  Z%(ZI%,1) = 0
  R%=ZI%
  ZI%=ZI%+1
  RETURN

REM ASSOC1(HM%, K%, V%) -> R%
ASSOC1:
  R%=ZI%
  REM key ptr
  Z%(ZI%,0) = 10
  Z%(ZI%,1) = ZI%+2: REM value
  ZI%=ZI%+1
  Z%(ZI%,0) = 15
  Z%(ZI%,1) = K%
  ZI%=ZI%+1
  REM value ptr
  Z%(ZI%,0) = 10
  Z%(ZI%,1) = HM%: REM hashmap to assoc onto
  ZI%=ZI%+1
  Z%(ZI%,0) = 15
  Z%(ZI%,1) = V%
  ZI%=ZI%+1
  RETURN

REM ASSOC1(HM%, K$, V%) -> R%
ASSOC1_S:
  REM add the key string, then call ASSOC1
  K%=ZI%
  ZS$(ZJ%) = K$
  Z%(ZI%,0) = 4
  Z%(ZI%,1) = ZJ%
  ZI%=ZI%+1
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
      IF Z%(T2%,0)=15 THEN T2%=Z%(T2%,1): GOTO HASHMAP_GET_DEREF
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
  Z%(ZI%,0) = 12
  Z%(ZI%,1) = A%
  R%=ZI%
  ZI%=ZI%+1
  RETURN

REM NATIVE_FUNCTION(A%, P%, E%) -> R%
MAL_FUNCTION:
  Z%(ZI%,0) = 13
  Z%(ZI%,1) = A%
  Z%(ZI%+1,0) = P%
  Z%(ZI%+1,1) = E%
  R%=ZI%
  ZI%=ZI%+2
  RETURN
