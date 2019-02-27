REM Memory layout:
REM
REM type            bytes
REM ----------      ----------
REM nil             ref/ 0 |  0          |              |
REM false           ref/ 1 |  0          |              |
REM true            ref/ 1 |  1          |              |
REM integer         ref/ 2 | int         |              |
REM float           ref/ 3 | ???         |              |
REM string/kw       ref/ 4 | S$ idx      |              |
REM symbol          ref/ 5 | S$ idx      |              |
REM list            ref/ 6 | next Z% idx | val Z% idx   |
REM vector          ref/ 7 | next Z% idx | val Z% idx   |
REM hashmap         ref/ 8 | next Z% idx | key Z% idx   | val Z% idx
REM function        ref/ 9 | fn idx      |              |
REM mal function    ref/10 | body Z% idx | param Z% idx | env Z% idx
REM macro fn        ref/11 | body Z% idx | param Z% idx | env Z% idx
REM atom            ref/12 | val Z% idx  |              |
REM environment     ref/13 | hmap Z% idx | outer Z% idx |
REM metadata        ref/14 | obj Z% idx  | meta Z% idx  |
REM FREE             sz/15 | next Z% idx |              |
REM
REM Locations 0-15 are for constant/persistent values:
REM    0: nil
REM    2: false
REM    4: true
REM    6: empty list
REM    9: empty vector
REM   12: empty hash-map

REM Note: DIM_MEMORY for C64 BASIC and the INIT_MEMORY function are at
REM end of this file for efficiency on C64. The most commonly used
REM function should be at the top since C64 BASIC scans line numbers
REM for every GOTO/GOSUB. On the other hand, QBasic requires that
REM arrays are dimensioned at the top of the file, not just as the
REM first operation on that array so DIM_MEMORY for QBasic is here at
REM the top.

#qbasic DIM_MEMORY:
#qbasic   T=0
#qbasic
#qbasic   Z1=8191+1424: REM Z% (boxed memory) size (2 bytes each)
#qbasic   Z2=199: REM S$/S% (string memory) size (3+2 bytes each)
#qbasic   Z3=200: REM X% (call stack) size (2 bytes each)
#qbasic   Z4=64: REM Y% (release stack) size (4 bytes each)
#qbasic
#qbasic   REM boxed element memory
#qbasic   DIM Z%(Z1): REM TYPE ARRAY
#qbasic
#qbasic   REM string memory storage
#qbasic   S=0:DIM S$(Z2):DIM S%(Z2)
#qbasic
#qbasic   REM call/logic stack
#qbasic   X=-1:DIM X%(Z3): REM stack of Z% indexes
#qbasic
#qbasic   REM pending release stack
#qbasic   Y=-1:DIM Y%(Z4,1): REM stack of Z% indexes and level/LV values
#qbasic
#qbasic   RETURN

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
REM ALLOC(T,L,M) -> R
REM ALLOC(T,L,M,N) -> R
REM L is value for Z%(R+1)
REM M is value for Z%(R+2), if SZ>2
REM N is value for Z%(R+3), if SZ>3
ALLOC:
  SZ=3
  IF T<6 OR T=9 OR T=12 THEN SZ=2
  IF T=8 OR T=10 OR T=11 THEN SZ=4
  REM PRINT "ALLOC T: "+STR$(T)+", SZ: "+STR$(SZ)+", ZK: "+STR$(ZK)
  U=ZK
  R=ZK
  ALLOC_LOOP:
    IF R=ZI THEN GOTO ALLOC_UNUSED
    REM TODO sanity check that type is 15
    IF ((Z%(R)AND-32)/32)=SZ THEN GOTO ALLOC_MIDDLE
    REM PRINT "ALLOC search: U: "+STR$(U)+", R: "+STR$(R)
    U=R: REM previous set to current
    R=Z%(R+1): REM current set to next
    GOTO ALLOC_LOOP
  ALLOC_MIDDLE:
    REM PRINT "ALLOC_MIDDLE: U: "+STR$(U)+", R: "+STR$(R)
    REM set free pointer (ZK) to next free
    IF R=ZK THEN ZK=Z%(R+1)
    REM set previous free to next free
    IF R<>ZK THEN Z%(U+1)=Z%(R+1)
    GOTO ALLOC_DONE
  ALLOC_UNUSED:
    REM PRINT "ALLOC_UNUSED ZI: "+STR$(ZI)+", U: "+STR$(U)+", R: "+STR$(R)
    IF R+SZ>Z1 THEN GOSUB PR_MEMORY_SUMMARY_SMALL:PRINT "Out of mal memory!":END
    ZI=ZI+SZ
    IF U=R THEN ZK=ZI
    REM set previous free to new memory top
    IF U<>R THEN Z%(U+1)=ZI
    GOTO ALLOC_DONE
  ALLOC_DONE:
    Z%(R)=T+32
    REM set Z%(R+1) to default L
    Z%(R+1)=L
    IF T>5 AND T<>9 THEN Z%(L)=Z%(L)+32: REM value is a Z% idx
    IF SZ>2 THEN Z%(M)=Z%(M)+32:Z%(R+2)=M
    IF SZ>3 THEN Z%(N)=Z%(N)+32:Z%(R+3)=N

    RETURN

REM FREE(AY, SZ) -> nil
FREE:
  REM assumes reference count cleanup already (see RELEASE)
  Z%(AY)=(SZ*32)+15: REM set type(15) and size
  Z%(AY+1)=ZK
  ZK=AY
  IF SZ>=3 THEN Z%(AY+2)=0
  IF SZ=4 THEN Z%(AY+3)=0
  REM TODO: fail if SZ>4
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

  U=Z%(AY)AND 31: REM type
  V=Z%(AY+1): REM main value/reference

  REM set the size
  REM TODO: share with ALLOC calculation
  SZ=3
  IF U<6 OR U=9 OR U=12 THEN SZ=2
  IF U=8 OR U=10 OR U=11 THEN SZ=4

  REM AZ=AY: B=1: GOSUB PR_STR
  REM PRINT "RELEASE AY:"+STR$(AY)+" ["+R$+"] (byte0:"+STR$(Z%(AY))+", SZ:"+STR$(SZ)+")"

  REM sanity check not already freed
  REM MEMORY DEBUGGING:
  REM IF U=15 THEN PRINT "RELEASE of free:"+STR$(AY):END
  REM IF Z%(AY)<15 THEN PRINT "RELEASE of unowned:"+STR$(AY):END

  REM decrease reference count by one
  Z%(AY)=Z%(AY)-32

  REM nil, false, true, empty sequences
  REM MEMORY DEBUGGING:
  REM IF AY<16 AND Z%(AY)<32 THEN PRINT "RELEASE of empty:"+STR$(AY):END
  IF AY<16 THEN GOTO RELEASE_TOP

  REM our reference count is not 0, so don't release
  IF Z%(AY)>=32 GOTO RELEASE_TOP

  REM switch on type
  ON U+1 GOSUB RELEASE_SIMPLE,RELEASE_SIMPLE,RELEASE_SIMPLE,RELEASE_SIMPLE,RELEASE_STRING,RELEASE_STRING,RELEASE_SEQ,RELEASE_SEQ,RELEASE_HASH_MAP,RELEASE_SIMPLE,RELEASE_MAL_FUNCTION,RELEASE_MAL_FUNCTION,RELEASE_ATOM,RELEASE_ENV,RELEASE_METADATA

  REM free the current element and continue, SZ already set
  GOSUB FREE
  GOTO RELEASE_TOP

  RELEASE_SIMPLE:
    RETURN
  RELEASE_STRING:
    REM string type, release interned string, then FREE reference
    REM MEMORY DEBUGGING:
    REM IF S%(V)=0 THEN PRINT "RELEASE of free string:"+STR$(S%(V)):END
    S%(V)=S%(V)-1
    IF S%(V)=0 THEN S$(V)="": REM free BASIC string
    REM free the atom itself
    RETURN
  RELEASE_SEQ:
    IF V=0 THEN RETURN
    REM add value and next element to stack
    RC=RC+2
    Q=Z%(AY+2):GOSUB PUSH_Q
    Q=V:GOSUB PUSH_Q
    RETURN
  RELEASE_HASH_MAP:
    IF V=0 THEN RETURN
    REM add key, value and next element to stack
    RC=RC+3
    Q=Z%(AY+2):GOSUB PUSH_Q
    Q=Z%(AY+3):GOSUB PUSH_Q
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
    Q=Z%(AY+2):GOSUB PUSH_Q
    Q=Z%(AY+3):GOSUB PUSH_Q
    REM free the current 3 element mal_function
    RETURN
  RELEASE_ENV:
    REM add the hashmap data to the stack
    RC=RC+1
    Q=V:GOSUB PUSH_Q
    REM if outer set, add outer env to stack
    IF Z%(AY+2)<>0 THEN RC=RC+1:Q=Z%(AY+2):GOSUB PUSH_Q
    RETURN
  RELEASE_METADATA:
    REM add object and metadata object
    RC=RC+2
    Q=V:GOSUB PUSH_Q
    Q=Z%(AY+2):GOSUB PUSH_Q
    RETURN


REM INC_REF_R(R) -> R
REM   - return R with 1 ref cnt increase
REM   - call with GOTO to return at caller callsite
REM   - call with GOSUB to return to caller
INC_REF_R:
  Z%(R)=Z%(R)+32
  RETURN

REM RETURN_TRUE_FALSE(R) -> R
REM   - take BASIC true/false R, return mal true/false R with ref cnt
REM   - called with GOTO as a return RETURN
RETURN_TRUE_FALSE:
  IF R THEN R=4
  IF R=0 THEN R=2
  GOTO INC_REF_R


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



#cbm DIM_MEMORY:
#cbm   T=FRE(0)
#cbm
#cbm   Z1=8191+1424: REM Z% (boxed memory) size (2 bytes each)
#cbm   Z2=199: REM S$/S% (string memory) size (3+2 bytes each)
#cbm   Z3=49152: REM X starting point at $C000 (2 bytes each)
#cbm   Z4=52992: REM Y starting point at $CF00 (4 bytes each)
#cbm
#cbm   REM TODO: for performance, define all/most non-array variables here
#cbm   REM so that the array area doesn't have to be shifted down everytime
#cbm   REM a new non-array variable is defined
#cbm
#cbm   REM boxed element memory
#cbm   DIM Z%(Z1): REM TYPE ARRAY
#cbm
#cbm   REM string memory storage
#cbm   S=0:DIM S$(Z2):DIM S%(Z2)
#cbm
#cbm   REM call/logic stack
#cbm   X=Z3-2: REM stack of 1920 Z% indexes at $C000
#cbm
#cbm   REM pending release stack
#cbm   Y=Z4-4: REM stack of 64 Y% indexes/levels at $CF00
#cbm
#cbm   RETURN

INIT_MEMORY:
  GOSUB DIM_MEMORY

  REM global error state
  REM  -2 : no error
  REM  -1 : string error in E$
  REM >=0 : pointer to error object
  ER=-2
  E$=""

  REM Predefine nil, false, true, and an empty sequences
  FOR I=0 TO 15:Z%(I)=0:NEXT I
  Z%(0)=32: REM nil
  Z%(2)=1+32: REM false
  Z%(4)=1+32:Z%(5)=1: REM true
  Z%(6)=6+32: REM emtpy list
  Z%(9)=7+32: REM empty vector
  Z%(12)=8+32: REM empty hash-map

  REM start of unused memory
  ZI=16

  REM start of free list
  ZK=16

  REM start of time clock
  #cbm BT=TI
  #qbasic BT#=TIMER(0.001)

  RETURN


