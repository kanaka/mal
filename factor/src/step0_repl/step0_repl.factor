! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io readline kernel system ;
IN: step0_repl

: READ ( x -- x ) ;
: EVAL ( x -- x ) ;
: PRINT ( x -- x ) ;
: rep ( x -- x ) READ EVAL PRINT ;

: main-loop ( -- )
            [ 1 ]
            [ "user> " readline
              [ 0 exit ] unless*
              rep print flush ]
            while ;

MAIN: main-loop
