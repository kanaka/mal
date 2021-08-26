! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit
continuations fry grouping hashtables io kernel lists locals lib.env lib.printer
lib.reader lib.types math namespaces quotations readline sequences vectors ;
IN: step3_env

CONSTANT: repl-bindings H{
    { "+" [ + ] }
    { "-" [ - ] }
    { "*" [ * ] }
    { "/" [ / ] }
}

SYMBOL: repl-env

DEFER: EVAL

:: eval-def! ( key value env -- maltype )
    value env EVAL [ key env env-set ] keep ;

: eval-let* ( bindings body env -- maltype )
    [ swap 2 group ] [ new-env ] bi* [
        dup '[ first2 _ EVAL swap _ env-set ] each
    ] keep EVAL ;

: READ ( str -- maltype ) read-str ;

: apply ( maltype env -- maltype )
    dup quotation? [ drop "not a fn" throw ] unless
    with-datastack
    first ;

GENERIC#: EVAL-switch 1 ( maltype env -- maltype )
M: array EVAL-switch
    over empty? [ drop ] [
        over first dup malsymbol? [ name>> ] when {
            { "def!" [ [ rest first2 ] dip eval-def! ] }
            { "let*" [ [ rest first2 ] dip eval-let* ] }
            [ drop '[ _ EVAL ] map unclip apply ]
        } case
    ] if ;
M: malsymbol EVAL-switch env-get ;
M: vector    EVAL-switch '[ _ EVAL ] map ;
M: hashtable EVAL-switch '[ _ EVAL ] assoc-map ;
M: object    EVAL-switch drop ;

: EVAL ( maltype env -- maltype )
    "DEBUG-EVAL" <malsymbol> over env-find [
        { f +nil+ } index not
        [
            "EVAL: " pick pr-str append print flush
        ] when
    ] [ drop ] if
    EVAL-switch ;

: PRINT ( maltype -- str ) pr-str ;

: REP ( str -- str )
    [
        READ repl-env get EVAL PRINT
    ] [
        nip pr-str "Error: " swap append
    ] recover ;

: REPL ( -- )
    f repl-bindings <malenv> repl-env set
    [
        "user> " readline [
            [ REP print flush ] unless-empty
        ] keep
    ] loop ;

MAIN: REPL
