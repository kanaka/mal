! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit
continuations fry io kernel math lib.printer lib.reader lib.types
quotations readline sequences ;
IN: step2_eval

CONSTANT: repl-env H{
    { "+" [ + ] }
    { "-" [ - ] }
    { "*" [ * ] }
    { "/" [ / ] }
}

DEFER: EVAL

GENERIC# eval-ast 1 ( ast env -- ast )
M: malsymbol eval-ast
    [ name>> ] dip ?at [ "no variable " prepend throw ] unless ;
M: sequence  eval-ast '[ _ EVAL ] map ;
M: assoc     eval-ast '[ _ EVAL ] assoc-map ;
M: object    eval-ast drop ;

: READ ( str -- maltype ) read-str ;

: EVAL ( maltype env -- maltype )
    eval-ast dup { [ array? ] [ empty? not ] } 1&& [
        unclip
        dup quotation? [ "not a fn" throw ] unless
        with-datastack first
    ] when ;

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
