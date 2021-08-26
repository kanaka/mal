! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit
command-line continuations fry grouping hashtables io kernel lists locals
lib.core lib.env lib.printer lib.reader lib.types math namespaces quotations
readline sequences splitting vectors ;
IN: step6_file

SYMBOL: repl-env

DEFER: EVAL

:: eval-def! ( key value env -- maltype )
    value env EVAL [ key env env-set ] keep ;

: eval-let* ( bindings body env -- maltype env )
    [ swap 2 group ] [ new-env ] bi* [
        dup '[ first2 _ EVAL swap _ env-set ] each
    ] keep ;

:: eval-do ( exprs env -- lastform env/f )
    exprs [
        { } f
    ] [
        unclip-last [ '[ env EVAL drop ] each ] dip env
    ] if-empty ;

:: eval-if ( params env -- maltype env/f )
    params first env EVAL { f +nil+ } index not [
        params second env
    ] [
        params length 2 > [ params third env ] [ nil f ] if
    ] if ;

:: eval-fn* ( params env -- maltype )
    env params first [ name>> ] map params second <malfn> ;

: args-split ( bindlist -- bindlist restbinding/f )
    { "&" } split1 ?first ;

: make-bindings ( args bindlist restbinding/f -- bindingshash )
    swapd [ over length cut [ zip ] dip ] dip
    [ swap 2array suffix ] [ drop ] if* >hashtable ;

GENERIC: apply ( args fn -- maltype newenv/f )

M: malfn apply
    [ exprs>> nip ]
    [ env>> nip ]
    [ binds>> args-split make-bindings ] 2tri <malenv> ;

M: callable apply call( x -- y ) f ;

: READ ( str -- maltype ) read-str ;

GENERIC#: EVAL-switch 1 ( maltype env -- maltype )
M: array EVAL-switch
    over empty? [ drop ] [
        over first dup malsymbol? [ name>> ] when {
            { "def!" [ [ rest first2 ] dip eval-def! f ] }
            { "let*" [ [ rest first2 ] dip eval-let* ] }
            { "do" [ [ rest ] dip eval-do ] }
            { "if" [ [ rest ] dip eval-if ] }
            { "fn*" [ [ rest ] dip eval-fn* f ] }
            [ drop '[ _ EVAL ] map unclip apply ]
        } case [ EVAL ] when*
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

[ apply [ EVAL ] when* ] mal-apply set-global

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

: main ( -- )
    command-line get
    [ REPL ]
    [ first "(load-file \"" "\")" surround REP drop ]
    if-empty ;

f ns clone
[ first repl-env get EVAL ] "eval" pick set-at
command-line get dup empty? [ rest ] unless "*ARGV*" pick set-at
<malenv> repl-env set-global

"
(def! not (fn* (a) (if a false true)))
(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))
" string-lines harvest [ REP drop ] each

MAIN: main
