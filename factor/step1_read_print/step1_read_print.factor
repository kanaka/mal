! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: continuations io kernel lib.printer lib.reader readline
sequences ;
IN: step1_read_print

: READ ( str -- maltype ) read-str ;

: EVAL ( maltype -- maltype ) ;

: PRINT ( maltype -- str ) pr-str ;

: REP ( str -- str )
    [ READ EVAL ] [ nip ] recover PRINT ;

: REPL ( -- )
    [
        "user> " readline [
            [ REP print flush ] unless-empty
        ] keep
    ] loop ;

MAIN: REPL
