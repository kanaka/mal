! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit
continuations fry hashtables io kernel math lib.printer lib.reader lib.types
quotations readline sequences vectors ;
IN: step2_eval

CONSTANT: repl-env H{
    { "+" [ + ] }
    { "-" [ - ] }
    { "*" [ * ] }
    { "/" [ / ] }
}

DEFER: EVAL

: READ ( str -- maltype ) read-str ;

: apply ( maltype env -- maltype )
    dup quotation? [ drop "not a fn" throw ] unless
    with-datastack
    first ;

GENERIC#: EVAL-switch 1 ( maltype env -- maltype )
M: array EVAL-switch
    '[ _ EVAL ] map
    dup empty? [ unclip apply ] unless ;
M: malsymbol EVAL-switch
    [ name>> ] dip ?at [ "no variable " prepend throw ] unless ;
M: vector    EVAL-switch '[ _ EVAL ] map ;
M: hashtable EVAL-switch '[ _ EVAL ] assoc-map ;
M: object    EVAL-switch drop ;

: EVAL ( maltype env -- maltype )
    ! "EVAL: " pick pr-str append print flush
    EVAL-switch ;

: PRINT ( maltype -- str ) pr-str ;

: REP ( str -- str )
    [
        READ repl-env EVAL PRINT
    ] [
        nip pr-str "Error: " swap append
    ] recover ;

: REPL ( -- )
    [
        "user> " readline [
            [ REP print flush ] unless-empty
        ] keep
    ] loop ;

MAIN: REPL
