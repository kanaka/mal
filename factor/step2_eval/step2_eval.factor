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

: eval-symbol ( sym env -- ast )
    [ name>> ] dip ?at [ "no variable " prepend throw ] unless ;

: eval-list ( list env -- ast )
    '[ _ EVAL ] map ;

: eval-assoc ( assoc env -- ast )
    '[ [ _ EVAL ] bi@ ] assoc-map ;

: eval-ast ( ast env -- ast )
    {
        { [ over malsymbol? ] [ eval-symbol ] }
        { [ over sequence? ]  [ eval-list ] }
        { [ over assoc? ]     [ eval-assoc ] }
        [ drop ]
    } cond ;

: READ ( str -- maltype ) read-str ;

: EVAL ( maltype env -- maltype )
    eval-ast dup { [ array? ] [ empty? not ] } 1&& [
        unclip
        dup quotation? [ "not a fn" throw ] unless
        with-datastack first
    ] when ;

: PRINT ( maltype -- str ) pr-str ;

: REP ( str -- str )
    [ READ repl-env EVAL ] [ nip ] recover PRINT ;

: REPL ( -- )
    [
        "user> " readline [
            [ REP print flush ] unless-empty
        ] keep
    ] loop ;

MAIN: REPL
