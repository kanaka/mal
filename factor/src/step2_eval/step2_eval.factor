! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io readline kernel system reader printer continuations types arrays locals assocs sequences
       combinators accessors fry quotations math ;
IN: step2_eval

CONSTANT: repl-env H{ { "+" [ + ] }
                      { "-" [ - ] }
                      { "*" [ * ] }
                      { "/" [ / ] } }

DEFER: EVAL

: eval-symbol ( sym env -- ast )
    [ name>> ] dip
    ?at [ dup "no variable " prepend throw ] unless ;

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
    eval-ast
    dup array?
    [ unclip
      dup quotation? [ "not a fn" throw ] unless
      with-datastack first ]
    when ;
: PRINT ( maltype -- str ) pr-str ;
: rep ( x -- x ) [ READ repl-env EVAL PRINT ] [ nip ] recover ;

: main-loop ( -- )
            [ 1 ]
            [ "user> " readline
              [ 0 exit ] unless*
              rep print flush ]
            while ;

MAIN: main-loop
