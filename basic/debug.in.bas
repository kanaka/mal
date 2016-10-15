REM CHECK_FREE_LIST
CHECK_FREE_LIST:
  REM start and accumulator
  P1%=ZK
  P2%=0
  CHECK_FREE_LIST_LOOP:
    IF P1%>=ZI THEN GOTO CHECK_FREE_LIST_DONE
    IF (Z%(P1%,0)AND15)<>15 THEN P2%=-1:GOTO CHECK_FREE_LIST_DONE
    P2%=P2%+(Z%(P1%,0)AND-16)/16
    P1%=Z%(P1%,1)
    GOTO CHECK_FREE_LIST_LOOP
  CHECK_FREE_LIST_DONE:
    IF P2%=-1 THEN PRINT "corrupt free list at "+STR$(P1%)
    RETURN

PR_MEMORY_SUMMARY:
  GOSUB CHECK_FREE_LIST: REM get count in P2%
  PRINT
  PRINT "Free memory (FRE)      : "+STR$(FRE(0))
  PRINT "Value memory (Z%)      : "+STR$(ZI-1)+" /"+STR$(Z1)
  PRINT "                         ";
  PRINT " used:"+STR$(ZI-1-P2%)+", freed:"+STR$(P2%);
  PRINT ", post repl_env:"+STR$(ZT%)
  PRINT "String values (S$)    : "+STR$(ZJ)+" /"+STR$(Z2)
  PRINT "Call stack size (S%)  : "+STR$(X+1)+" /"+STR$(Z3)
  RETURN

REM REM PR_MEMORY(P1%, P2%) -> nil
REM PR_MEMORY:
REM   IF P2%<P1% THEN P2%=ZI-1
REM   PRINT "vvvvvv"
REM   PRINT "Z% Value Memory"+STR$(P1%)+"->"+STR$(P2%);
REM   PRINT " (ZI: "+STR$(ZI)+", ZK: "+STR$(ZK)+"):"
REM   IF P2%<P1% THEN PRINT "  ---":GOTO PR_MEMORY_AFTER_VALUES
REM   I=P1%
REM   PR_MEMORY_VALUE_LOOP:
REM     IF I>P2% THEN GOTO PR_MEMORY_AFTER_VALUES
REM     PRINT " "+STR$(I);
REM     IF (Z%(I,0)AND15)=15 THEN GOTO PR_MEMORY_FREE
REM       PRINT ": ref cnt: "+STR$((Z%(I,0)AND-16)/16);
REM       PRINT ", type: "+STR$(Z%(I,0)AND15)+", value: "+STR$(Z%(I,1));
REM       IF (Z%(I,0)AND15)=4 THEN PRINT "    '"+S$(Z%(I,1))+"'";
REM       IF (Z%(I,0)AND15)=5 THEN PRINT "    "+S$(Z%(I,1))+"";
REM       PRINT
REM       I=I+1
REM       IF (Z%(I-1,0)AND15)<>10 THEN GOTO PR_MEMORY_VALUE_LOOP
REM         PRINT " "+STR$(I)+":            ";
REM         PRINT "params: "+STR$(Z%(I+1,0))+", env:"+STR$(Z%(I+1,1))
REM         I=I+1
REM       GOTO PR_MEMORY_VALUE_LOOP
REM     PR_MEMORY_FREE:
REM       PRINT ": FREE size: "+STR$((Z%(I,0)AND-16)/16)+", next: "+STR$(Z%(I,1));
REM       IF I=ZK THEN PRINT " (free list start)";
REM       PRINT
REM       IF (Z%(I,0)AND-16)=32 THEN I=I+1:PRINT " "+STR$(I)+": ---"
REM       I=I+1
REM       GOTO PR_MEMORY_VALUE_LOOP
REM   PR_MEMORY_AFTER_VALUES:
REM   PRINT "ZS% String Memory (ZJ: "+STR$(ZJ)+"):"
REM   IF ZJ<=0 THEN PRINT "  ---":GOTO PR_MEMORY_SKIP_STRINGS
REM   FOR I=0 TO ZJ-1
REM     PRINT " "+STR$(I)+": '"+S$(I)+"'"
REM     NEXT I
REM   PR_MEMORY_SKIP_STRINGS:
REM   PRINT "S% Stack Memory (X: "+STR$(X)+"):"
REM   IF X<0 THEN PRINT "  ---":GOTO PR_MEMORY_SKIP_STACK
REM   FOR I=0 TO X
REM     PRINT " "+STR$(I)+": "+STR$(S%(I))
REM     NEXT I
REM   PR_MEMORY_SKIP_STACK:
REM   PRINT "^^^^^^"
REM   RETURN
REM 
REM REM PR_OBJECT(P1%) -> nil
REM PR_OBJECT:
REM   RD%=0
REM 
REM   RD%=RD%+1:X=X+1:S%(X)=P1%
REM 
REM   PR_OBJ_LOOP:
REM     IF RD%=0 THEN RETURN
REM     I=S%(X):RD%=RD%-1:X=X-1
REM 
REM     P2%=Z%(I,0)AND15
REM     PRINT " "+STR$(I);
REM     PRINT ": ref cnt: "+STR$((Z%(I,0)AND-16)/16);
REM     PRINT ", type: "+STR$(P2%)+", value: "+STR$(Z%(I,1));
REM     IF P2%=4 THEN PRINT "    '"+S$(Z%(I,1))+"'";
REM     IF P2%=5 THEN PRINT "    "+S$(Z%(I,1))+"";
REM     PRINT
REM     IF P2%<=5 OR P2%=9 THEN GOTO PR_OBJ_LOOP
REM     IF Z%(I,1)<>0 THEN RD%=RD%+1:X=X+1:S%(X)=Z%(I,1)
REM     IF P2%>=6 AND P2%<=8 THEN RD%=RD%+1:X=X+1:S%(X)=I+1
REM     GOTO PR_OBJ_LOOP
