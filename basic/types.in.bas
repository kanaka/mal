REM                  Z 0   ->  1
REM nil                0   ->  (unused)
REM boolean            1   ->  0: false, 1: true
REM integer            2   ->  int value
REM float              3   ->  ???
REM string/kw          4   ->  S$ index
REM symbol             5   ->  S$ index
REM list next/val      6   ->  next Z% index (0 for last)
REM                    14      value (unless empty)
REM vector next/val    7   ->  next Z% index (0 for last)
REM                    14      value (unless empty)
REM hashmap next/val   8   ->  next Z% index (0 for last)
REM                    key     value
REM function           9   ->  function index
REM mal function       10  ->  body AST Z% index
REM                    param   env Z% index
REM macro (same as 10) 11  ->  body AST Z% index
REM                    param   env Z% index
REM atom               12  ->  Z% index
REM environment        13  ->  data/hashmap Z% index
REM                    14      outer Z% index (-1 for none)
REM reference/ptr      14  ->  Z% index / or 0
REM next free ptr      15  ->  Z% index / or 0
REM metadata        16-31  ->  Z% index of object with this metadata
REM                    14  ->  Z% index of metdata object

INIT_MEMORY:
  #cbm T=FRE(0)
  #qbasic T=0

  Z1=4195: REM Z% (boxed memory) size (4 bytes each)
  Z2=199: REM S$/S% (string memory) size (3+2 bytes each)
  #qbasic Z3=200: REM X% (call stack) size (2 bytes each)
  #cbm Z3=49152: REM X starting point at $C000 (2 bytes each)
  #qbasic Z4=64: REM Y% (release stack) size (4 bytes each)
  #cbm Z4=52992: REM Y starting point at $CF00 (4 bytes each)

  REM global error state
  REM  -2 : no error
  REM  -1 : string error in E$
  REM >=0 : pointer to error object
  ER=-2
  E$=""

  REM TODO: for performance, define all/most non-array variables here
  REM so that the array area doesn't have to be shifted down everytime
  REM a new non-array variable is defined

  REM boxed element memory
  DIM Z%(Z1,1): REM TYPE ARRAY

  REM Predefine nil, false, true, and an empty list
  FOR I=0 TO 8:Z%(I,0)=32:Z%(I,1)=0:NEXT I
  Z%(1,0)=1+32
  Z%(2,0)=1+32:Z%(2,1)=1
  Z%(3,0)=6+32:Z%(3,1)=0
  Z%(5,0)=7+32:Z%(5,1)=0
  Z%(7,0)=8+32:Z%(7,1)=0

REM  Z%(0,0)=0:Z%(0,1)=0
REM  Z%(1,0)=1:Z%(1,1)=0
REM  Z%(2,0)=1:Z%(2,1)=1
REM  Z%(3,0)=6+32:Z%(3,1)=0
REM  Z%(4,0)=0:Z%(4,1)=0
REM  Z%(5,0)=7+32:Z%(5,1)=0
REM  Z%(6,0)=0:Z%(6,1)=0
REM  Z%(7,0)=8+32:Z%(7,1)=0
REM  Z%(8,0)=0:Z%(8,1)=0

  REM start of unused memory
  ZI=9

  REM start of free list
  ZK=9

  REM string memory storage
  S=0:DIM S$(Z2):DIM S%(Z2)

  REM call/logic stack
  #qbasic X=-1:DIM X%(Z3): REM stack of Z% indexes
  #cbm X=Z3-2: REM stack of 1920 Z% indexes at $C000

  REM pending release stack
  #qbasic Y=-1:DIM Y%(Z4,1): REM stack of Z% indexes and level/LV values
  #cbm Y=Z4-4: REM stack of 64 Y% indexes/levels at $CF00

  BT=TI

  RETURN


REM stack functions

#qbasic PUSH_A:
#qbasic   X=X+1:X%(X)=A:RETURN
#qbasic POP_A:
#qbasic   A=X%(X):X=X-1:RETURN
#qbasic 
#qbasic PUSH_R:
#qbasic   X=X+1:X%(X)=R:RETURN
#qbasic POP_R:
#qbasic   R=X%(X):X=X-1:RETURN
#qbasic 
#qbasic PUSH_Q:
#qbasic   X=X+1:X%(X)=Q:RETURN
#qbasic POP_Q:
#qbasic   Q=X%(X):X=X-1:RETURN
#qbasic PEEK_Q:
#qbasic   Q=X%(X):RETURN
#qbasic PEEK_Q_1:
#qbasic   Q=X%(X-1):RETURN
#qbasic PEEK_Q_2:
#qbasic   Q=X%(X-2):RETURN
#qbasic PEEK_Q_Q:
#qbasic   Q=X%(X-Q):RETURN
#qbasic PUT_Q:
#qbasic   X%(X)=Q:RETURN
#qbasic PUT_Q_1:
#qbasic   X%(X-1)=Q:RETURN
#qbasic PUT_Q_2:
#qbasic   X%(X-2)=Q:RETURN

#cbm PUSH_A:
#cbm   X=X+2:POKE X,A AND255:POKE X+1,A/256:RETURN
#cbm POP_A:
#cbm   A=PEEK(X)+PEEK(X+1)*256:X=X-2:RETURN
#cbm 
#cbm PUSH_R:
#cbm   X=X+2:POKE X,R AND255:POKE X+1,R/256:RETURN
#cbm POP_R:
#cbm   R=PEEK(X)+PEEK(X+1)*256:X=X-2:RETURN
#cbm 
#cbm PUSH_Q:
#cbm   X=X+2:POKE X,Q AND255:POKE X+1,Q/256:RETURN
#cbm POP_Q:
#cbm   Q=PEEK(X)+PEEK(X+1)*256:X=X-2:RETURN
#cbm PEEK_Q:
#cbm   Q=PEEK(X)+PEEK(X+1)*256:RETURN
#cbm PEEK_Q_1:
#cbm   Q=PEEK(X-2)+PEEK(X-1)*256:RETURN
#cbm PEEK_Q_2:
#cbm   Q=PEEK(X-4)+PEEK(X-3)*256:RETURN
#cbm PEEK_Q_Q:
#cbm   Q=PEEK(X-Q*2)+PEEK(X-Q*2+1)*256:RETURN
#cbm PUT_Q:
#cbm   POKE X,Q AND255:POKE X+1,Q/256:RETURN
#cbm PUT_Q_1:
#cbm   POKE X-2,Q AND255:POKE X-1,Q/256:RETURN
#cbm PUT_Q_2:
#cbm   POKE X-4,Q AND255:POKE X-3,Q/256:RETURN

REM memory functions

REM ALLOC(T,L) -> R
REM ALLOC(T,L,N) -> R
REM ALLOC(T,L,M,N) -> R
REM L is default for Z%(R,1)
REM M is default for Z%(R+1,0), if relevant for T
REM N is default for Z%(R+1,1), if relevant for T
ALLOC:
  SZ=2
  IF T<6 OR T=9 OR T=12 OR T=14 THEN SZ=1
  REM PRINT "ALLOC T: "+STR$(T)+", SZ: "+STR$(SZ)+", ZK: "+STR$(ZK)
  U=ZK
  R=ZK
  ALLOC_LOOP:
    IF R=ZI THEN GOTO ALLOC_UNUSED
    REM TODO sanity check that type is 15
    IF ((Z%(R,0)AND-32)/32)=SZ THEN GOTO ALLOC_MIDDLE
    REM PRINT "ALLOC search: U: "+STR$(U)+", R: "+STR$(R)
    U=R: REM previous set to current
    R=Z%(R,1): REM current set to next
    GOTO ALLOC_LOOP
  ALLOC_MIDDLE:
    REM PRINT "ALLOC_MIDDLE: U: "+STR$(U)+", R: "+STR$(R)
    REM set free pointer (ZK) to next free
    IF R=ZK THEN ZK=Z%(R,1)
    REM set previous free to next free
    IF R<>ZK THEN Z%(U,1)=Z%(R,1)
    GOTO ALLOC_DONE
  ALLOC_UNUSED:
    REM PRINT "ALLOC_UNUSED ZI: "+STR$(ZI)+", U: "+STR$(U)+", R: "+STR$(R)
    IF R+SZ>Z1 THEN PRINT "Out of mal memory!":END
    ZI=ZI+SZ
    IF U=R THEN ZK=ZI
    REM set previous free to new memory top
    IF U<>R THEN Z%(U,1)=ZI
    GOTO ALLOC_DONE
  ALLOC_DONE:
    Z%(R,0)=T+32
    REM set Z%(R,1) to default L
    IF T>=6 AND T<>9 THEN Z%(L,0)=Z%(L,0)+32
    Z%(R,1)=L

    IF SZ=1 THEN RETURN
    Z%(R+1,0)=14: REM default for 6-8, and 13, and >=16 (metadata)

    REM function/macro/hash-map sets Z%(R+1,0) to default M
    IF T=8 OR T=10 OR T=11 THEN Z%(M,0)=Z%(M,0)+32:Z%(R+1,0)=M

    REM seq, function/macro, environment sets Z%(R+1,1) to default N
    Z%(N,0)=Z%(N,0)+32
    Z%(R+1,1)=N
    RETURN

REM FREE(AY, SZ) -> nil
FREE:
  REM assumes reference count cleanup already (see RELEASE)
  Z%(AY,0)=(SZ*32)+15: REM set type(15) and size
  Z%(AY,1)=ZK
  ZK=AY
  IF SZ>=2 THEN Z%(AY+1,0)=0:Z%(AY+1,1)=0
  IF SZ>=3 THEN Z%(AY+2,0)=0:Z%(AY+2,1)=0
  RETURN


REM RELEASE(AY) -> nil
REM R should not be affected by this call
RELEASE:
  RC=0

  GOTO RELEASE_ONE

  RELEASE_TOP:

  IF RC=0 THEN RETURN

  REM pop next object to release, decrease remaining count
  GOSUB POP_Q:AY=Q
  RC=RC-1

  RELEASE_ONE:
  IF AY=-1 THEN RETURN

  U=Z%(AY,0)AND 31: REM type
  V=Z%(AY,1): REM main value/reference

  REM AZ=AY: B=1: GOSUB PR_STR
  REM PRINT "RELEASE AY:"+STR$(AY)+"["+R$+"] (byte0:"+STR$(Z%(AY,0))+")"

  REM sanity check not already freed
  IF (U)=15 THEN PRINT "RELEASE of free:"+STR$(AY):END
  IF Z%(AY,0)<15 THEN PRINT "RELEASE of unowned:"+STR$(AY):END

  REM decrease reference count by one
  Z%(AY,0)=Z%(AY,0)-32

  REM nil, false, true, empty sequences
  IF AY<9 AND Z%(AY,0)<32 THEN PRINT "RELEASE of empty:"+STR$(AY):END
  IF AY<9 THEN GOTO RELEASE_TOP

  REM our reference count is not 0, so don't release
  IF Z%(AY,0)>=32 GOTO RELEASE_TOP

  REM switch on type
  SZ=1: REM default FREE size, adjusted by RELEASE_*
  IF U>=16 THEN GOSUB RELEASE_METADATA

REM  IF U<=3 OR U=9 THEN GOSUB RELEASE_SIMPLE
REM  IF U=4 OR U=5 THEN GOSUB RELEASE_STRING
REM  IF U>=6 AND U<=8 THEN GOSUB RELEASE_SEQ
REM  IF U=10 OR U=11 THEN GOSUB RELEASE_MAL_FUNCTION
REM  IF U>=16 THEN GOSUB RELEASE_METADATA
REM  IF U=12 THEN GOSUB RELEASE_ATOM
REM  IF U=13 THEN GOSUB RELEASE_ENV

  ON U+1 GOSUB RELEASE_SIMPLE,RELEASE_SIMPLE,RELEASE_SIMPLE,RELEASE_SIMPLE,RELEASE_STRING,RELEASE_STRING,RELEASE_SEQ,RELEASE_SEQ,RELEASE_HASH_MAP,RELEASE_SIMPLE,RELEASE_MAL_FUNCTION,RELEASE_MAL_FUNCTION,RELEASE_ATOM,RELEASE_ENV

  REM free the current element and continue, SZ already set
  GOSUB FREE
  GOTO RELEASE_TOP

  RELEASE_SIMPLE:
    RETURN
  RELEASE_STRING:
    REM string type, release interned string, then FREE reference
    IF S%(V)=0 THEN ER=-1:E$="RELEASE of free string:"+STR$(S%(V)):RETURN
    S%(V)=S%(V)-1
    IF S%(V)=0 THEN S$(V)="": REM free BASIC string
    REM free the atom itself
    RETURN
  RELEASE_SEQ:
    SZ=2
    IF V=0 THEN RETURN
    IF Z%(AY+1,0)<>14 THEN ER=-1:E$="invalid list value"+STR$(AY+1):RETURN
    REM add value and next element to stack
    RC=RC+2
    Q=Z%(AY+1,1):GOSUB PUSH_Q
    Q=V:GOSUB PUSH_Q
    RETURN
  RELEASE_HASH_MAP:
    SZ=2
    IF V=0 THEN RETURN
    REM add key, value and next element to stack
    RC=RC+3
    Q=Z%(AY+1,0):GOSUB PUSH_Q
    Q=Z%(AY+1,1):GOSUB PUSH_Q
    Q=V:GOSUB PUSH_Q
    RETURN
  RELEASE_ATOM:
    REM add contained/referred value
    RC=RC+1
    Q=V:GOSUB PUSH_Q
    REM free the atom itself
    RETURN
  RELEASE_MAL_FUNCTION:
    REM add ast, params and environment to stack
    RC=RC+3
    Q=V:GOSUB PUSH_Q
    Q=Z%(AY+1,0):GOSUB PUSH_Q
    Q=Z%(AY+1,1):GOSUB PUSH_Q
    REM free the current 2 element mal_function and continue
    SZ=2:RETURN
  RELEASE_METADATA:
    REM add object and metadata object
    RC=RC+2
    Q=V:GOSUB PUSH_Q
    Q=Z%(AY+1,1):GOSUB PUSH_Q
    SZ=2:RETURN
  RELEASE_ENV:
    REM add the hashmap data to the stack
    RC=RC+1
    Q=V:GOSUB PUSH_Q
    REM if outer set, add outer env to stack
    IF Z%(AY+1,1)<>0 THEN RC=RC+1:Q=Z%(AY+1,1):GOSUB PUSH_Q
    REM add outer environment to the stack
    SZ=2:RETURN


REM release stack functions

#qbasic PEND_A_LV:
#qbasic   Y=Y+1:Y%(Y,0)=A:Y%(Y,1)=LV:RETURN
#qbasic
#qbasic REM RELEASE_PEND(LV) -> nil
#qbasic RELEASE_PEND:
#qbasic   IF Y<0 THEN RETURN
#qbasic   IF Y%(Y,1)<=LV THEN RETURN
#qbasic   REM PRINT "RELEASE_PEND releasing:"+STR$(Y%(Y,0))
#qbasic   AY=Y%(Y,0):GOSUB RELEASE
#qbasic   Y=Y-1
#qbasic   GOTO RELEASE_PEND

#cbm PEND_A_LV:
#cbm   Y=Y+4:POKE Y,A AND255:POKE Y+1,A/256
#cbm         POKE Y+2,LV AND255:POKE Y+3,LV/256:RETURN
#cbm
#cbm REM RELEASE_PEND(LV) -> nil
#cbm RELEASE_PEND:
#cbm   IF Y<Z4 THEN RETURN
#cbm   IF (PEEK(Y+2)+PEEK(Y+3)*256)<=LV THEN RETURN
#cbm   REM PRINT "RELEASE_PEND releasing:"+STR$(Y%(Y,0))
#cbm   AY=(PEEK(Y)+PEEK(Y+1)*256):GOSUB RELEASE
#cbm   Y=Y-4
#cbm   GOTO RELEASE_PEND

REM VAL_R(R) -> R
VAL_R:
  R=Z%(R+1,1)
  RETURN

REM VAL_A(A) -> A
VAL_A:
  A=Z%(A+1,1)
  RETURN

REM VAL_B(B) -> B
VAL_B:
  B=Z%(B+1,1)
  RETURN


REM general functions

REM EQUAL_Q(A, B) -> R
EQUAL_Q:
  ED=0: REM recursion depth
  R=-1: REM return value

  EQUAL_Q_RECUR:

  REM push A and B
  GOSUB PUSH_A
  Q=B:GOSUB PUSH_Q
  ED=ED+1

  T1=Z%(A,0)AND 31
  T2=Z%(B,0)AND 31
  IF T1>5 AND T1<8 AND T2>5 AND T2<8 THEN GOTO EQUAL_Q_SEQ
  IF T1=8 AND T2=8 THEN GOTO EQUAL_Q_HM

  IF T1<>T2 OR Z%(A,1)<>Z%(B,1) THEN R=0
  GOTO EQUAL_Q_DONE

  EQUAL_Q_SEQ:
    IF Z%(A,1)=0 AND Z%(B,1)=0 THEN GOTO EQUAL_Q_DONE
    IF Z%(A,1)=0 OR Z%(B,1)=0 THEN R=0:GOTO EQUAL_Q_DONE

    REM compare the elements
    A=Z%(A+1,1):B=Z%(B+1,1)
    GOTO EQUAL_Q_RECUR

  EQUAL_Q_SEQ_CONTINUE:
    REM next elements of the sequences
    GOSUB PEEK_Q_1:A=Q
    GOSUB PEEK_Q:B=Q
    A=Z%(A,1):B=Z%(B,1)
    Q=A:GOSUB PUT_Q_1
    Q=B:GOSUB PUT_Q
    GOTO EQUAL_Q_SEQ

  EQUAL_Q_HM:
    R=0
    GOTO EQUAL_Q_DONE

  EQUAL_Q_DONE:
    REM pop current A and B
    GOSUB POP_Q
    GOSUB POP_Q
    ED=ED-1
    IF R>-1 AND ED>0 THEN GOTO EQUAL_Q_DONE: REM unwind
    IF ED=0 AND R=-1 THEN R=1
    IF ED=0 THEN RETURN
    GOTO EQUAL_Q_SEQ_CONTINUE

REM string functions

REM STRING(B$, T) -> R
REM intern string and allocate reference (return Z% index)
STRING:
  IF S=0 THEN GOTO STRING_NOT_FOUND

  REM search for matching string in S$
  I=0
  STRING_FIND_LOOP:
    IF I>S-1 THEN GOTO STRING_NOT_FOUND
    IF S%(I)>0 AND B$=S$(I) THEN GOTO STRING_DONE
    I=I+1
    GOTO STRING_FIND_LOOP

  STRING_NOT_FOUND:
    I=S-1
    STRING_FIND_GAP_LOOP:
      REM TODO: don't search core function names (store position)
      IF I=-1 THEN GOTO STRING_NEW
      IF S%(I)=0 THEN GOTO STRING_SET
      I=I-1
      GOTO STRING_FIND_GAP_LOOP

  STRING_NEW:
    I=S
    S=S+1
    REM fallthrough

  STRING_SET:
    S$(I)=B$
    REM fallthrough

  STRING_DONE:
    S%(I)=S%(I)+1
    L=I:GOSUB ALLOC
    RETURN

REM REPLACE(R$, S1$, S2$) -> R$
REPLACE:
  T3$=R$
  R$=""
  I=1
  J=LEN(T3$)
  REPLACE_LOOP:
    IF I>J THEN RETURN
    C$=MID$(T3$,I,LEN(S1$))
    IF C$=S1$ THEN R$=R$+S2$:I=I+LEN(S1$)
    IF C$<>S1$ THEN R$=R$+MID$(T3$,I,1):I=I+1
    GOTO REPLACE_LOOP


REM sequence functions

REM FORCE_SEQ_TYPE(A,T) -> R
FORCE_SEQ_TYPE:
  REM if it's already the right type, inc ref cnt and return it
  IF (Z%(A,0)AND 31)=T THEN R=A:Z%(R,0)=Z%(R,0)+32:RETURN
  REM if it's empty, return the empty sequence match T
  IF A<9 THEN R=(T-5)*2+1:Z%(R,0)=Z%(R,0)+32:RETURN
  REM otherwise, copy first element to turn it into correct type
  B=A:GOSUB VAL_B: REM value to copy
  L=Z%(A,1):N=B:GOSUB ALLOC: REM T already set
  IF Z%(A,1)=0 THEN RETURN
  RETURN

REM MAP_LOOP_START(T):
REM   - setup stack for map loop
MAP_LOOP_START:
  REM point to empty sequence to start off
  R=(T-5)*2+1: REM calculate location of empty seq
  Z%(R,0)=Z%(R,0)+32

  GOSUB PUSH_R: REM push return ptr
  GOSUB PUSH_R: REM push empty ptr
  GOSUB PUSH_R: REM push current ptr
  RETURN

REM MAP_LOOP_UPDATE(C,N):
REM MAP_LOOP_UPDATE(C,M,N):
REM  - called after N (and M if T=8) are set
REM  - C indicates whether to free N (and M if T=8)
REM  - update the structure of the return sequence
MAP_LOOP_UPDATE:
  GOSUB PEEK_Q_1:L=Q: REM empty ptr

  GOSUB ALLOC: REM allocate new sequence element

  REM sequence took ownership
  AY=L:GOSUB RELEASE
  IF C THEN AY=N:GOSUB RELEASE
  IF C AND T=8 THEN AY=M:GOSUB RELEASE

  REM if not first element, set current next to point to new element
  GOSUB PEEK_Q
  IF Q>8 THEN Z%(Q,1)=R
  REM if first element, set return to new element
  IF Q<9 THEN Q=R:GOSUB PUT_Q_2
  Q=R:GOSUB PUT_Q: REM update current ptr to new element

  RETURN

REM MAP_LOOP_DONE() -> R
REM   - cleanup stack and set return value
MAP_LOOP_DONE:
  GOSUB POP_Q: REM pop current ptr
  GOSUB POP_Q: REM pop empty ptr
  GOSUB POP_R: REM pop return ptr
  RETURN


REM LIST_Q(A) -> R
LIST_Q:
  R=0
  IF (Z%(A,0)AND 31)=6 THEN R=1
  RETURN

REM EMPTY_Q(A) -> R
EMPTY_Q:
  R=0
  IF Z%(A,1)=0 THEN R=1
  RETURN

REM COUNT(A) -> R
REM - returns length of list, not a Z% index
COUNT:
  GOSUB PUSH_A
  R=-1
  DO_COUNT_LOOP:
    R=R+1
    IF Z%(A,1)<>0 THEN A=Z%(A,1):GOTO DO_COUNT_LOOP
  GOSUB POP_A
  RETURN

REM LAST(A) -> R
LAST:
  REM TODO check that actually a list/vector
  IF Z%(A,1)=0 THEN R=0:RETURN: REM empty seq, return nil
  W=0
  LAST_LOOP:
    IF Z%(A,1)=0 THEN GOTO LAST_DONE: REM end, return previous value
    W=A: REM current becomes previous entry
    A=Z%(A,1): REM next entry
    GOTO LAST_LOOP
  LAST_DONE:
    R=W:GOSUB VAL_R
    Z%(R,0)=Z%(R,0)+32
    RETURN

REM SLICE(A,B,C) -> R
REM make copy of sequence A from index B to C
REM returns R6 as reference to last element of slice before empty
REM returns A as next element following slice (of original)
SLICE:
  I=0
  R=3: REM always a list
  Z%(R,0)=Z%(R,0)+32
  R6=-1: REM last list element before empty
  W=R: REM temporary for return as R
  REM advance A to position B
  SLICE_FIND_B:
    IF I<B AND Z%(A,1)<>0 THEN A=Z%(A,1):I=I+1:GOTO SLICE_FIND_B
  SLICE_LOOP:
    REM if current position is C, then return
    IF C<>-1 AND I>=C THEN R=W:RETURN
    REM if we reached end of A, then return
    IF Z%(A,1)=0 THEN R=W:RETURN
    REM allocate new list element with copied value
    T=6:L=3:N=Z%(A+1,1):GOSUB ALLOC
    REM sequence took ownership
    AY=L:GOSUB RELEASE
    REM if not first element, set last to point to new element
    IF R6>-1 THEN Z%(R6,1)=R
    REM if first element, set return value to new element
    IF R6=-1 THEN W=R
    R6=R: REM update last list element
    REM advance to next element of A
    A=Z%(A,1)
    I=I+1
    GOTO SLICE_LOOP

REM LIST2(B,A) -> R
LIST2:
  REM last element is 3 (empty list), second element is A
  T=6:L=3:N=A:GOSUB ALLOC

  REM first element is B
  T=6:L=R:N=B:GOSUB ALLOC
  AY=L:GOSUB RELEASE: REM new list takes ownership of previous

  RETURN

REM LIST3(C,B,A) -> R
LIST3:
  GOSUB LIST2

  REM first element is C
  T=6:L=R:N=C:GOSUB ALLOC
  AY=L:GOSUB RELEASE: REM new list takes ownership of previous

  RETURN


REM hashmap functions

REM HASHMAP() -> R
HASHMAP:
  REM just point to static empty hash-map
  R=7
  Z%(R,0)=Z%(R,0)+32
  RETURN

REM ASSOC1(H, K, C) -> R
ASSOC1:
  REM create key/value entry
  T=8:L=H:M=K:N=C:GOSUB ALLOC
  AY=L:GOSUB RELEASE: REM we took ownership of previous hashmap
  RETURN

REM ASSOC1_S(H, B$, C) -> R
ASSOC1_S:
  REM add the key string
  T=4:GOSUB STRING
  K=R:GOSUB ASSOC1
  AY=K:GOSUB RELEASE: REM map took ownership of key
  RETURN

REM HASHMAP_GET(H, K) -> R
REM   - returns R3 with whether we found it or not
HASHMAP_GET:
  B$=S$(Z%(K,1)): REM search key string
  R3=0: REM whether found or not (for HASHMAP_CONTAINS)
  R=0
  HASHMAP_GET_LOOP:
    REM no matching key found
    IF Z%(H,1)=0 THEN R=0:RETURN
    REM get search string is equal to key string we found it
    IF B$=S$(Z%(Z%(H+1,0),1)) THEN R3=1:R=Z%(H+1,1):RETURN
    REM skip to next key/value
    H=Z%(H,1)
    GOTO HASHMAP_GET_LOOP

REM HASHMAP_CONTAINS(H, K) -> R
HASHMAP_CONTAINS:
  GOSUB HASHMAP_GET
  R=R3
  RETURN


REM function functions

REM NATIVE_FUNCTION(A) -> R
NATIVE_FUNCTION:
  T=9:L=A:GOSUB ALLOC
  RETURN

REM MAL_FUNCTION(A, B, E) -> R
MAL_FUNCTION:
  T=10:L=A:M=B:N=E:GOSUB ALLOC
  RETURN
