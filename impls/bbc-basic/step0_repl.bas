REM Step 0 of mal in BBC BASIC

REPEAT
  PRINT "user> ";
  LINE INPUT "" line$
  PRINT FNrep(line$)
UNTIL FALSE

END

DEF FNREAD(a$)
=a$

DEF FNEVAL(a$)
=a$

DEF FNPRINT(a$)
=a$

DEF FNrep(a$)
=FNPRINT(FNEVAL(FNREAD(a$)))

REM Local Variables:
REM indent-tabs-mode: nil
REM End:
