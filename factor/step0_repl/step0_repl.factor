! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io kernel readline sequences ;
IN: step0_repl

: READ ( x -- x ) ;

: EVAL ( x -- x ) ;

: PRINT ( x -- x ) ;

: REP ( x -- x ) READ EVAL PRINT ;

: REPL ( -- )
    [
        "user> " readline [
             [ REP print flush ] unless-empty
        ] keep
    ] loop ;

MAIN: REPL
