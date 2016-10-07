PR_MEMORY_SUMMARY:
  GOSUB CHECK_FREE_LIST: REM get count in P2%
  PRINT
  PRINT "Free memory (FRE)      : "+STR$(FRE(0))
  PRINT "Value memory (Z%)      : "+STR$(ZI%-1)+" /"+STR$(S1%)
  PRINT "                         ";
  PRINT " used:"+STR$(ZI%-1-P2%)+", freed:"+STR$(P2%);
  PRINT ", post repl_env:"+STR$(ZT%)
  PRINT "String values (ZS$)    : "+STR$(ZJ%)+" /"+STR$(S2%)
  PRINT "Call stack size (ZZ%)  : "+STR$(ZL%+1)+" /"+STR$(S3%)
  RETURN

REM REM PR_MEMORY(P1%, P2%) -> nil
REM PR_MEMORY:
REM   IF P2%<P1% THEN P2%=ZI%-1
REM   PRINT "vvvvvv"
REM   PRINT "Z% Value Memory"+STR$(P1%)+"->"+STR$(P2%);
REM   PRINT " (ZI%: "+STR$(ZI%)+", ZK%: "+STR$(ZK%)+"):"
REM   IF P2%<P1% THEN PRINT "  ---":GOTO PR_MEMORY_AFTER_VALUES
REM   I=P1%
REM   PR_MEMORY_VALUE_LOOP:
REM     IF I>P2% THEN GOTO PR_MEMORY_AFTER_VALUES
REM     PRINT " "+STR$(I);
REM     IF (Z%(I,0)AND15)=15 THEN GOTO PR_MEMORY_FREE
REM       PRINT ": ref cnt: "+STR$((Z%(I,0)AND-16)/16);
REM       PRINT ", type: "+STR$(Z%(I,0)AND15)+", value: "+STR$(Z%(I,1));
REM       IF (Z%(I,0)AND15)=4 THEN PRINT "    '"+ZS$(Z%(I,1))+"'";
REM       IF (Z%(I,0)AND15)=5 THEN PRINT "    "+ZS$(Z%(I,1))+"";
REM       PRINT
REM       I=I+1
REM       IF (Z%(I-1,0)AND15)<>10 THEN GOTO PR_MEMORY_VALUE_LOOP
REM         PRINT " "+STR$(I)+":            ";
REM         PRINT "params: "+STR$(Z%(I+1,0))+", env:"+STR$(Z%(I+1,1))
REM         I=I+1
REM       GOTO PR_MEMORY_VALUE_LOOP
REM     PR_MEMORY_FREE:
REM       PRINT ": FREE size: "+STR$((Z%(I,0)AND-16)/16)+", next: "+STR$(Z%(I,1));
REM       IF I=ZK% THEN PRINT " (free list start)";
REM       PRINT
REM       IF (Z%(I,0)AND-16)=32 THEN I=I+1:PRINT " "+STR$(I)+": ---"
REM       I=I+1
REM       GOTO PR_MEMORY_VALUE_LOOP
REM   PR_MEMORY_AFTER_VALUES:
REM   PRINT "ZS% String Memory (ZJ%: "+STR$(ZJ%)+"):"
REM   IF ZJ%<=0 THEN PRINT "  ---":GOTO PR_MEMORY_SKIP_STRINGS
REM   FOR I=0 TO ZJ%-1
REM     PRINT " "+STR$(I)+": '"+ZS$(I)+"'"
REM     NEXT I
REM   PR_MEMORY_SKIP_STRINGS:
REM   PRINT "ZZ% Stack Memory (ZL%: "+STR$(ZL%)+"):"
REM   IF ZL%<0 THEN PRINT "  ---":GOTO PR_MEMORY_SKIP_STACK
REM   FOR I=0 TO ZL%
REM     PRINT " "+STR$(I)+": "+STR$(ZZ%(I))
REM     NEXT I
REM   PR_MEMORY_SKIP_STACK:
REM   PRINT "^^^^^^"
REM   RETURN
REM 
REM REM PR_OBJECT(P1%) -> nil
REM PR_OBJECT:
REM   RD%=0
REM 
REM   RD%=RD%+1:ZL%=ZL%+1:ZZ%(ZL%)=P1%
REM 
REM   PR_OBJ_LOOP:
REM     IF RD%=0 THEN RETURN
REM     I=ZZ%(ZL%):RD%=RD%-1:ZL%=ZL%-1
REM 
REM     P2%=Z%(I,0)AND15
REM     PRINT " "+STR$(I);
REM     PRINT ": ref cnt: "+STR$((Z%(I,0)AND-16)/16);
REM     PRINT ", type: "+STR$(P2%)+", value: "+STR$(Z%(I,1));
REM     IF P2%=4 THEN PRINT "    '"+ZS$(Z%(I,1))+"'";
REM     IF P2%=5 THEN PRINT "    "+ZS$(Z%(I,1))+"";
REM     PRINT
REM     IF P2%<=5 OR P2%=9 THEN GOTO PR_OBJ_LOOP
REM     IF Z%(I,1)<>0 THEN RD%=RD%+1:ZL%=ZL%+1:ZZ%(ZL%)=Z%(I,1)
REM     IF P2%>=6 AND P2%<=8 THEN RD%=RD%+1:ZL%=ZL%+1:ZZ%(ZL%)=I+1
REM     GOTO PR_OBJ_LOOP
