REM CHECK_FREE_LIST() -> P2
CHECK_FREE_LIST:
  REM start and accumulator
  P1=ZK
  P2=0
  CHECK_FREE_LIST_LOOP:
    IF P1>=ZI THEN GOTO CHECK_FREE_LIST_DONE
    IF (Z%(P1,0)AND 31)<>15 THEN P2=-1:GOTO CHECK_FREE_LIST_DONE
    P2=P2+(Z%(P1,0)AND-32)/32
    P1=Z%(P1,1)
    GOTO CHECK_FREE_LIST_LOOP
  CHECK_FREE_LIST_DONE:
    IF P2=-1 THEN PRINT "corrupt free list at "+STR$(P1)
    RETURN

PR_MEMORY_SUMMARY_SMALL:
  #cbm P0=FRE(0)

  GOSUB CHECK_FREE_LIST
  #cbm PRINT "Free:"+STR$(FRE(0))+", ";
  PRINT "Values:"+STR$(ZI-1-P2)+", Emptys:";
  FOR I=0 TO 7
    IF I<>4 AND I<>6 THEN PRINT STR$(INT(Z%(I,0)/32))+",";
    NEXT I
  PRINT
  RETURN

REM REM COUNT_STRINGS() -> P2
REM COUNT_STRINGS:
REM   P1=0
REM   P2=0
REM   COUNT_STRINGS_LOOP:
REM     IF P1>S-1 THEN RETURN
REM     IF S%(P1)>0 THEN P2=P2+1
REM     P1=P1+1
REM     GOTO COUNT_STRINGS_LOOP
REM 
REM PR_MEMORY_SUMMARY:
REM   #cbm P0=FRE(0)
REM 
REM   PRINT
REM   #cbm PRINT "Free (FRE)   :"+STR$(P0)
REM   GOSUB CHECK_FREE_LIST: REM get count in P2
REM   PRINT "Values (Z%)  :"+STR$(ZI-1-P2)+" /"+STR$(Z1)
REM   REM PRINT "               max:"+STR$(ZI-1);
REM   REM PRINT ", freed:"+STR$(P2)+", after repl_env:"+STR$(ZT)
REM   GOSUB COUNT_STRINGS
REM   PRINT "Strings (S$) :"+STR$(P2)+" /"+STR$(Z2)
REM   #qbasic PRINT "Stack (X%)   :"+STR$(X+1)+" /"+STR$(Z3)
REM   #cbm PRINT "Stack        :"+STR$(X+2-Z3)+" / 1920"
REM   RETURN
REM 
REM #cbm PR_MEMORY_MAP:
REM   #cbm PRINT
REM   #cbm P1=PEEK(43)+PEEK(44)*256
REM   #cbm P2=PEEK(45)+PEEK(46)*256
REM   #cbm P3=PEEK(47)+PEEK(48)*256
REM   #cbm P4=PEEK(49)+PEEK(50)*256
REM   #cbm P5=PEEK(51)+PEEK(52)*256
REM   #cbm P6=PEEK(53)+PEEK(54)*256
REM   #cbm P7=PEEK(55)+PEEK(56)*256
REM   #cbm PRINT "BASIC beg.   :"STR$(P1)
REM   #cbm PRINT "Variable beg.:"STR$(P2)
REM   #cbm PRINT "Array beg.   :"STR$(P3)
REM   #cbm PRINT "Array end    :"STR$(P4)
REM   #cbm PRINT "String beg.  :"STR$(P5)
REM   #cbm PRINT "String cur.  :"STR$(P6)
REM   #cbm PRINT "BASIC end    :"STR$(P7)
REM   #cbm PRINT
REM   #cbm PRINT "Program Code :"STR$(P2-P1)
REM   #cbm PRINT "Variables    :"STR$(P3-P2)
REM   #cbm PRINT "Arrays       :"STR$(P4-P3)
REM   #cbm PRINT "String Heap  :"STR$(P7-P5)
REM   #cbm RETURN
REM 
REM REM PR_MEMORY(P1, P2) -> nil
REM PR_MEMORY:
REM   IF P2<P1 THEN P2=ZI-1
REM   PRINT "vvvvvv"
REM   PRINT "Z% Value Memory"+STR$(P1)+"->"+STR$(P2);
REM   PRINT " (ZI: "+STR$(ZI)+", ZK: "+STR$(ZK)+"):"
REM   IF P2<P1 THEN PRINT "  ---":GOTO PR_MEMORY_AFTER_VALUES
REM   I=P1
REM   PR_MEMORY_VALUE_LOOP:
REM     IF I>P2 THEN GOTO PR_MEMORY_AFTER_VALUES
REM     PRINT " "+STR$(I);
REM     IF (Z%(I,0)AND 31)=15 THEN GOTO PR_MEMORY_FREE
REM       PRINT ": ref cnt: "+STR$((Z%(I,0)AND-32)/32);
REM       PRINT ", type: "+STR$(Z%(I,0)AND 31)+", value: "+STR$(Z%(I,1));
REM       IF (Z%(I,0)AND 31)=4 THEN PRINT "    '"+S$(Z%(I,1))+"'";
REM       IF (Z%(I,0)AND 31)=5 THEN PRINT "    "+S$(Z%(I,1))+"";
REM       PRINT
REM       I=I+1
REM       IF (Z%(I-1,0)AND 31)<>10 THEN GOTO PR_MEMORY_VALUE_LOOP
REM         PRINT " "+STR$(I)+":            ";
REM         PRINT "params: "+STR$(Z%(I+1,0))+", env:"+STR$(Z%(I+1,1))
REM         I=I+1
REM       GOTO PR_MEMORY_VALUE_LOOP
REM     PR_MEMORY_FREE:
REM       PRINT ": FREE size: "+STR$((Z%(I,0)AND-32)/32)+", next: "+STR$(Z%(I,1));
REM       IF I=ZK THEN PRINT " (free list start)";
REM       PRINT
REM       IF (Z%(I,0)AND-32)=64 THEN I=I+1:PRINT " "+STR$(I)+": ---"
REM       I=I+1
REM       GOTO PR_MEMORY_VALUE_LOOP
REM   PR_MEMORY_AFTER_VALUES:
REM   PRINT "S$ String Memory (S: "+STR$(S)+"):"
REM   IF S<=0 THEN PRINT "  ---":GOTO PR_MEMORY_SKIP_STRINGS
REM   FOR I=0 TO S-1
REM     PRINT " "+STR$(I)+": '"+S$(I)+"'"
REM     NEXT I
REM   PR_MEMORY_SKIP_STRINGS:
REM   PRINT "X% Stack Memory (X: "+STR$(X)+"):"
REM #cbm  IF X<Z3 THEN PRINT "  ---":GOTO PR_MEMORY_SKIP_STACK
REM #cbm  FOR I=Z3 TO X
REM #cbm    PRINT " "+STR$(I)+": "+STR$(PEEK(X)+PEEK(X+1)*256)
REM #cbm    NEXT I
REM #qbasic  IF X<0 THEN PRINT "  ---":GOTO PR_MEMORY_SKIP_STACK
REM #qbasic  FOR I=0 TO X
REM #qbasic    #qbasic PRINT " "+STR$(I)+": "+STR$(X%(I))
REM #qbasic    NEXT I
REM   PR_MEMORY_SKIP_STACK:
REM   PRINT "^^^^^^"
REM   RETURN
REM 
REM REM PR_OBJECT(P1) -> nil
REM PR_OBJECT:
REM   RD=0
REM 
REM   IF P1=-1 THEN PRINT "  "+STR$(-1)+": ---":RETURN
REM   RD=RD+1
REM   Q=P1:GOSUB PUSH_Q
REM 
REM   PR_OBJ_LOOP:
REM     IF RD=0 THEN RETURN
REM     GOSUB POP_Q:I=Q
REM     RD=RD-1
REM 
REM     P2=Z%(I,0)AND 31
REM     P3=Z%(I,1)
REM     PRINT " "+STR$(I);
REM     PRINT ": ref cnt:"+STR$((Z%(I,0)AND-32)/32);
REM     PRINT ", type:"+STR$(P2)+", value: "+STR$(Z%(I,1));
REM     IF P2=2 THEN PRINT "    "+STR$(Z%(I,1));
REM     IF P2=4 THEN PRINT "    '"+S$(Z%(I,1))+"'";
REM     IF P2=5 THEN PRINT "    "+S$(Z%(I,1));
REM     IF P2=6 AND P3>0 THEN PRINT "    ()";
REM     IF P2=6 AND P3=0 THEN PRINT "    )";
REM     IF P2=7 AND P3>0 THEN PRINT "    []";
REM     IF P2=7 AND P3=0 THEN PRINT "    ]";
REM     IF P2=8 AND P3>0 THEN PRINT "    {}";
REM     IF P2=8 AND P3=0 THEN PRINT "    }";
REM     IF P2=9 THEN PRINT "    #<fn"+STR$(P3)+">";
REM     PRINT
REM     IF P2=8 THEN PRINT " "+STR$(I+1)+":              key:"+STR$(Z%(I+1,0));
REM     IF P2=8 THEN PRINT ", value:"+STR$(Z%(I+1,1))
REM 
REM     IF P2<=5 OR P2=9 THEN GOTO PR_OBJ_LOOP
REM     IF Z%(I,1)<>0 THEN RD=RD+1:Q=Z%(I,1):GOSUB PUSH_Q
REM     IF P2=8 AND I<>7 THEN RD=RD+2:Q=Z%(I+1,1):GOSUB PUSH_Q:Q=Z%(I+1,0):GOSUB PUSH_Q
REM     IF P2>5 AND P2<8 THEN RD=RD+1:Q=I+1:GOSUB PUSH_Q
REM     GOTO PR_OBJ_LOOP
