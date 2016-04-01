! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit
command-line continuations fry grouping hashtables io kernel lists locals
lib.core lib.env lib.printer lib.reader lib.types math namespaces quotations
readline sequences splitting ;
IN: step6_file

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

: eval-let* ( bindings body env -- maltype env )
    [ swap 2 group ] [ new-env ] bi* [
        dup '[ first2 _ EVAL swap _ env-set ] each
    ] keep ;

:: eval-do ( exprs env -- lastform env/f )
    exprs [
        { } f
    ] [
        unclip-last [ env eval-ast drop ] dip env
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

: EVAL ( maltype env -- maltype )
    over { [ array? ] [ empty? not ] } 1&& [
        over first dup malsymbol? [ name>> ] when {
            { "def!" [ [ rest first2 ] dip eval-def! f ] }
            { "let*" [ [ rest first2 ] dip eval-let* ] }
            { "do" [ [ rest ] dip eval-do ] }
            { "if" [ [ rest ] dip eval-if ] }
            { "fn*" [ [ rest ] dip eval-fn* f ] }
            [ drop '[ _ EVAL ] map unclip apply ]
        } case [ EVAL ] when*
    ] [
        eval-ast
    ] if ;

[ apply [ EVAL ] when* ] mal-apply set-global

: PRINT ( maltype -- str ) pr-str ;

: REP ( str -- str )
    [ READ repl-env get EVAL ] [ nip ] recover PRINT ;

: REPL ( -- )
    [
        "user> " readline [
            [ REP print flush ] unless-empty
        ] keep
    ] loop ;

f ns clone
[ first repl-env get EVAL ] "eval" pick set-at
command-line get "*ARGV*" pick set-at
<malenv> repl-env set-global

"
(def! not (fn* (a) (if a false true)))
(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))
" string-lines harvest [ REP drop ] each

MAIN: REPL
