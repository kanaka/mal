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
  
  S1%=4096: REM Z% (boxed memory) size (X2)
  S2%=512: REM ZS% (string memory) size
  S3%=64: REM PS% (logic stack) size
  S4%=256: REM ZE% (environments) size
  S5%=512: REM ZZ% (call stack) size

  REM global error state
  ER%=0
  ER$=""

  REM boxed element memory
  DIM Z%(S1%,1): REM TYPE ARRAY

  REM Predefine nil, false, true
  Z%(0,0) = 0
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
  DIM ZE%(S4%): REM data hashmap Z% index
  DIM ZO%(S4%): REM outer ZE% index (or -1)

  REM call stack
  ZL%=-1
  DIM ZZ%(S5%): REM stack of Z% indexes

  REM logic stack
  PT%=-1: REM index of top of PS% stack
  DIM PS%(S3%): REM stack of Z% indexes

  REM PRINT "Lisp data memory: " + STR$(T%-FRE(0))
  REM PRINT "Interpreter working memory: " + STR$(FRE(0))
  RETURN

REM DEREF(R%) -> R%
DEREF:
  IF Z%(R%,0)=15 THEN R%=Z%(R%,1): GOTO DEREF
  RETURN


REM LIST functions

LIST_Q:
  R%=0
  IF Z%(A%,0)=6 THEN R%=1
  RETURN

EMPTY_Q:
  R%=0
  IF Z%(A%,1)=0 THEN R%=1
  RETURN

REM HASHMAP functions

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

MAL_FUNCTION:
  RETURN
