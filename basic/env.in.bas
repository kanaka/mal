
REM ENV_NEW(C) -> R
ENV_NEW:
  REM allocate the data hashmap
  GOSUB HASHMAP
  AY=R

  REM set the outer and data pointer
  T=13:L=R:M=C:GOSUB ALLOC
  GOSUB RELEASE: REM environment takes ownership
  RETURN

REM see RELEASE types.in.bas for environment cleanup

REM ENV_NEW_BINDS(C, A, B) -> R
ENV_NEW_BINDS:
  GOSUB ENV_NEW
  E=R
  REM process bindings
  ENV_NEW_BINDS_LOOP:
    IF Z%(A+1)=0 THEN R=E:RETURN
    REM get/deref the key from A
    K=Z%(A+2)

    IF S$(Z%(K+1))="&" THEN GOTO EVAL_NEW_BINDS_VARGS

    EVAL_NEW_BINDS_1x1:
      REM get/deref the key from B
      C=Z%(B+2)
      REM set the binding in the environment data
      GOSUB ENV_SET
      REM go to next element of A and B
      A=Z%(A+1)
      B=Z%(B+1)
      GOTO ENV_NEW_BINDS_LOOP

    EVAL_NEW_BINDS_VARGS:
      REM get/deref the key from next element of A
      A=Z%(A+1)
      K=Z%(A+2)
      REM the value is the remaining list in B
      A=B:T=6:GOSUB FORCE_SEQ_TYPE
      C=R
      REM set the binding in the environment data
      GOSUB ENV_SET
      R=E
      AY=C:GOSUB RELEASE: REM list is owned by environment
      RETURN

REM ENV_SET(E, K, C) -> R
ENV_SET:
  H=Z%(E+1)
  GOSUB ASSOC1
  Z%(E+1)=R
  R=C
  RETURN

REM ENV_SET_S(E, B$, C) -> R
ENV_SET_S:
  H=Z%(E+1)
  GOSUB ASSOC1_S
  Z%(E+1)=R
  R=C
  RETURN

REM ENV_FIND(E, K) -> R
REM   Returns environment (R) containing K. If found, value found is
REM   in R4
SUB ENV_FIND
  T=E
  ENV_FIND_LOOP:
    H=Z%(T+1)
    REM More efficient to use GET for value (R) and contains? (R3)
    GOSUB HASHMAP_GET
    REM if we found it, save value in R4 for ENV_GET
    IF R3=1 THEN R4=R:R=T:GOTO ENV_FIND_DONE
    T=Z%(T+2): REM get outer environment
    IF T>0 THEN GOTO ENV_FIND_LOOP
    R=-1
  ENV_FIND_DONE:
END SUB

REM ENV_GET(E, K) -> R
ENV_GET:
  CALL ENV_FIND
  IF R=-1 THEN ER=-1:E$="'"+S$(Z%(K+1))+"' not found":GOTO ENV_GET_RETURN
  R=R4
  GOSUB INC_REF_R
  GOTO ENV_GET_RETURN
