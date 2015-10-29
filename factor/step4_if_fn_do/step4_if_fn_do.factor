! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators continuations fry
grouping hashtables io kernel lists locals mal.core mal.env
mal.printer mal.reader mal.types math namespaces quotations
readline sequences splitting ;
IN: step4_if_fn_do

SYMBOL: repl-env

DEFER: EVAL

: eval-ast ( ast env -- ast )
    {
        { [ over malsymbol? ] [ env-get ] }
        { [ over sequence? ]  [ '[ _ EVAL ] map ] }
        { [ over assoc? ]     [ '[ [ _ EVAL ] bi@ ] assoc-map ] }
        [ drop ]
    } cond ;

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

:: EVAL ( maltype env -- maltype )
    maltype dup array? [
        dup first dup malsymbol? [ name>> ] when {
            { "def!" [ rest first2 env eval-def! ] }
            { "let*" [ rest first2 env eval-let* ] }
            { "do" [ rest env eval-ast last ] }
            { "if" [ rest env eval-if ] }
            { "fn*" [ rest env eval-fn* ] }
            [ drop [ env EVAL ] map unclip apply ]
        } case
    ] [
        env eval-ast
    ] if ;

: PRINT ( maltype -- str ) pr-str ;

: REP ( str -- str )
    [ READ repl-env get EVAL ] [ nip ] recover PRINT ;

: REPL ( -- )
    [
        "user> " readline [
            [ REP print flush ] unless-empty
        ] keep
    ] loop ;

f ns <malenv> repl-env set-global
"(def! not (fn* (a) (if a false true)))" REP drop

MAIN: REPL
