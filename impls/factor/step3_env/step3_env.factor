! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit
continuations fry grouping hashtables io kernel locals lib.env lib.printer
lib.reader lib.types math namespaces quotations readline sequences ;
IN: step3_env

CONSTANT: repl-bindings H{
    { "+" [ + ] }
    { "-" [ - ] }
    { "*" [ * ] }
    { "/" [ / ] }
}

SYMBOL: repl-env

DEFER: EVAL

GENERIC# eval-ast 1 ( ast env -- ast )
M: malsymbol eval-ast env-get ;
M: sequence  eval-ast '[ _ EVAL ] map ;
M: assoc     eval-ast '[ _ EVAL ] assoc-map ;
M: object    eval-ast drop ;

:: eval-def! ( key value env -- maltype )
    value env EVAL [ key env env-set ] keep ;

: eval-let* ( bindings body env -- maltype )
    [ swap 2 group ] [ new-env ] bi* [
        dup '[ first2 _ EVAL swap _ env-set ] each
    ] keep EVAL ;

: READ ( str -- maltype ) read-str ;

:: EVAL ( maltype env -- maltype )
    maltype dup { [ array? ] [ empty? not ] } 1&& [
        unclip dup dup malsymbol? [ name>> ] when {
            { "def!" [ drop first2 env eval-def! ] }
            { "let*" [ drop first2 env eval-let* ] }
            [
                drop env eval-ast dup quotation? [
                    [ env eval-ast ] dip with-datastack first
                ] [
                    drop "not a fn" throw
                ] if
            ]
        } case
    ] [
        env eval-ast
    ] if ;

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
