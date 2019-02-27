REM general functions

REM TYPE_A(A) -> T
TYPE_A:
  T=Z%(A)AND 31
  RETURN

REM TYPE_F(F) -> T
TYPE_F:
  T=Z%(F)AND 31
  RETURN

REM EQUAL_Q(A, B) -> R
EQUAL_Q:
  ED=0: REM recursion depth
  R=-1: REM return value

  EQUAL_Q_RECUR:

  REM push A and B
  GOSUB PUSH_A
  Q=B:GOSUB PUSH_Q
  ED=ED+1

  GOSUB TYPE_A
  T2=Z%(B)AND 31
  IF T>5 AND T<8 AND T2>5 AND T2<8 THEN GOTO EQUAL_Q_SEQ
  IF T=8 AND T2=8 THEN GOTO EQUAL_Q_HM

  IF T<>T2 OR Z%(A+1)<>Z%(B+1) THEN R=0
  GOTO EQUAL_Q_DONE

  EQUAL_Q_SEQ:
    IF Z%(A+1)=0 AND Z%(B+1)=0 THEN GOTO EQUAL_Q_DONE
    IF Z%(A+1)=0 OR Z%(B+1)=0 THEN R=0:GOTO EQUAL_Q_DONE

    REM compare the elements
    A=Z%(A+2):B=Z%(B+2)
    GOTO EQUAL_Q_RECUR

  EQUAL_Q_SEQ_CONTINUE:
    REM next elements of the sequences
    GOSUB PEEK_Q_1:A=Q
    GOSUB PEEK_Q:B=Q
    A=Z%(A+1):B=Z%(B+1)
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
  IF (Z%(A)AND 31)=T THEN R=A:GOTO INC_REF_R
  REM if it's empty, return the empty sequence match T
  IF A<16 THEN R=(T-4)*3:GOTO INC_REF_R
  REM otherwise, copy first element to turn it into correct type
  B=Z%(A+2): REM value to copy
  L=Z%(A+1):M=B:GOSUB ALLOC: REM T already set
  IF Z%(A+1)=0 THEN RETURN
  RETURN

REM MAP_LOOP_START(T):
REM   - setup stack for map loop
MAP_LOOP_START:
  REM point to empty sequence to start off
  R=(T-4)*3: REM calculate location of empty seq

  GOSUB PUSH_R: REM push return ptr
  GOSUB PUSH_R: REM push empty ptr
  GOSUB PUSH_R: REM push current ptr
  GOTO INC_REF_R

REM MAP_LOOP_UPDATE(C,M):
REM MAP_LOOP_UPDATE(C,M,N):
REM  - called after M (and N if T=8) are set
REM  - C indicates whether to free M (and N if T=8)
REM  - update the structure of the return sequence
MAP_LOOP_UPDATE:
  GOSUB PEEK_Q_1:L=Q: REM empty ptr

  GOSUB ALLOC: REM allocate new sequence element

  REM sequence took ownership
  AY=L:GOSUB RELEASE
  IF C THEN AY=M:GOSUB RELEASE
  IF C AND T=8 THEN AY=N:GOSUB RELEASE

  REM if not first element, set current next to point to new element
  GOSUB PEEK_Q
  IF Q>14 THEN Z%(Q+1)=R
  REM if first element, set return to new element
  IF Q<15 THEN Q=R:GOSUB PUT_Q_2
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
  GOSUB TYPE_A
  IF T=6 THEN R=1
  RETURN

REM EMPTY_Q(A) -> R
EMPTY_Q:
  R=0
  IF Z%(A+1)=0 THEN R=1
  RETURN

REM COUNT(A) -> R
REM - returns length of list, not a Z% index
COUNT:
  GOSUB PUSH_A
  R=-1
  DO_COUNT_LOOP:
    R=R+1
    IF Z%(A+1)<>0 THEN A=Z%(A+1):GOTO DO_COUNT_LOOP
  GOSUB POP_A
  RETURN

REM LAST(A) -> R
LAST:
  REM TODO check that actually a list/vector
  IF Z%(A+1)=0 THEN R=0:RETURN: REM empty seq, return nil
  W=0
  LAST_LOOP:
    IF Z%(A+1)=0 THEN GOTO LAST_DONE: REM end, return previous value
    W=A: REM current becomes previous entry
    A=Z%(A+1): REM next entry
    GOTO LAST_LOOP
  LAST_DONE:
    R=Z%(W+2)
    GOTO INC_REF_R

REM SLICE(A,B,C) -> R
REM make copy of sequence A from index B to C
REM returns R6 as reference to last element of slice before empty
REM returns A as next element following slice (of original)
SLICE:
  I=0
  R=6: REM always a list
  GOSUB INC_REF_R
  R6=-1: REM last list element before empty
  W=R: REM temporary for return as R
  REM advance A to position B
  SLICE_FIND_B:
    IF I<B AND Z%(A+1)<>0 THEN A=Z%(A+1):I=I+1:GOTO SLICE_FIND_B
  SLICE_LOOP:
    REM if current position is C, then return
    IF C<>-1 AND I>=C THEN R=W:RETURN
    REM if we reached end of A, then return
    IF Z%(A+1)=0 THEN R=W:RETURN
    REM allocate new list element with copied value
    T=6:L=6:M=Z%(A+2):GOSUB ALLOC
    REM sequence took ownership
    AY=L:GOSUB RELEASE
    REM if not first element, set last to point to new element
    IF R6>-1 THEN Z%(R6+1)=R
    REM if first element, set return value to new element
    IF R6=-1 THEN W=R
    R6=R: REM update last list element
    REM advance to next element of A
    A=Z%(A+1)
    I=I+1
    GOTO SLICE_LOOP

REM LIST2(B,A) -> R
LIST2:
  REM last element is 3 (empty list), second element is A
  T=6:L=6:M=A:GOSUB ALLOC

  REM first element is B
  T=6:L=R:M=B:GOSUB ALLOC
  AY=L:GOSUB RELEASE: REM new list takes ownership of previous

  RETURN

REM LIST3(C,B,A) -> R
LIST3:
  GOSUB LIST2

  REM first element is C
  T=6:L=R:M=C:GOSUB ALLOC
  AY=L:GOSUB RELEASE: REM new list takes ownership of previous

  RETURN


REM hashmap functions

REM HASHMAP() -> R
HASHMAP:
  REM just point to static empty hash-map
  R=12
  GOTO INC_REF_R

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
  B$=S$(Z%(K+1)): REM search key string
  R3=0: REM whether found or not (for HASHMAP_CONTAINS)
  R=0
  HASHMAP_GET_LOOP:
    REM no matching key found
    IF Z%(H+1)=0 THEN R=0:RETURN
    REM get search string is equal to key string we found it
    IF B$=S$(Z%(Z%(H+2)+1)) THEN R3=1:R=Z%(H+3):RETURN
    REM skip to next key/value
    H=Z%(H+1)
    GOTO HASHMAP_GET_LOOP

REM HASHMAP_CONTAINS(H, K) -> R
HASHMAP_CONTAINS:
  GOSUB HASHMAP_GET
  R=R3
  RETURN

