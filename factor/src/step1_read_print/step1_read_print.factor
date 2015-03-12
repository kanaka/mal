! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io readline kernel system reader printer continuations ;
IN: step1_read_print

: READ ( str -- maltype ) read-str ;
: EVAL ( maltype -- maltype ) ;
: PRINT ( maltype -- str ) pr-str ;
: rep ( x -- x ) [ READ EVAL PRINT ] [ nip ] recover ;

: main-loop ( -- )
            [ 1 ]
            [ "user> " readline
              [ 0 exit ] unless*
              rep print flush ]
            while ;

MAIN: main-loop
