! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit
continuations fry grouping hashtables io kernel lists locals lib.core lib.env
lib.printer lib.reader lib.types math namespaces quotations readline sequences
splitting vectors ;
IN: step4_if_fn_do

SYMBOL: repl-env

DEFER: EVAL

:: eval-def! ( key value env -- maltype )
    value env EVAL [ key env env-set ] keep ;

: eval-let* ( bindings body env -- maltype )
    [ swap 2 group ] [ new-env ] bi* [
        dup '[ first2 _ EVAL swap _ env-set ] each
    ] keep EVAL ;

:: eval-if ( params env -- maltype )
    params first env EVAL { f +nil+ } index not [
        params second env EVAL
    ] [
        params length 2 > [ params third env EVAL ] [ nil ] if
    ] if ;

:: eval-fn* ( params env -- maltype )
    env params first [ name>> ] map params second <malfn> ;

: args-split ( bindlist -- bindlist restbinding/f )
    { "&" } split1 ?first ;

: make-bindings ( args bindlist restbinding/f -- bindingshash )
    swapd [ over length cut [ zip ] dip ] dip
    [ swap 2array suffix ] [ drop ] if* >hashtable ;

GENERIC: apply ( args fn -- maltype )

M: malfn apply
    [ exprs>> nip ]
    [ env>> nip ]
    [ binds>> args-split make-bindings ] 2tri <malenv> EVAL ;

M: callable apply call( x -- y ) ;

: READ ( str -- maltype ) read-str ;

GENERIC#: EVAL-switch 1 ( maltype env -- maltype )
M: array EVAL-switch
    over empty? [ drop ] [
        over first dup malsymbol? [ name>> ] when {
            { "def!" [ [ rest first2 ] dip eval-def! ] }
            { "let*" [ [ rest first2 ] dip eval-let* ] }
            { "do" [ [ rest ] dip '[ _ EVAL ] map last ] }
            { "if" [ [ rest ] dip eval-if ] }
            { "fn*" [ [ rest ] dip eval-fn* ] }
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
    [
        "user> " readline [
            [ REP print flush ] unless-empty
        ] keep
    ] loop ;

f ns <malenv> repl-env set-global
"(def! not (fn* (a) (if a false true)))" REP drop

MAIN: REPL
