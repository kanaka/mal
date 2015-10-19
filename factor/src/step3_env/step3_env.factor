! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io readline kernel system reader printer continuations types arrays locals assocs sequences
       combinators accessors fry quotations math malenv namespaces grouping hashtables ;
IN: step3_env

CONSTANT: repl-bindings H{ { "+" [ + ] }
                           { "-" [ - ] }
                           { "*" [ * ] }
                           { "/" [ / ] } }
SYMBOL: repl-env

DEFER: EVAL

: eval-ast ( ast env -- ast )
    {
        { [ over malsymbol? ] [ get-or-throw ] }
        { [ over sequence? ]  [ '[ _ EVAL ] map ] }
        { [ over assoc? ]     [ '[ [ _ EVAL ] bi@ ] assoc-map ] }
        [ drop ]
    } cond ;

:: eval-set! ( key value env -- maltype )
    value env EVAL [ key env set-at ] keep ;

:: eval-let* ( bindings body env -- maltype )
    body bindings 2 group env new-env
    [| env pair | pair first2 env EVAL swap env ?set-at ]
    reduce EVAL ;

: READ ( str -- maltype ) read-str ;
:: EVAL ( maltype env -- maltype )
    maltype dup array?
    [ unclip
      {
          { [ dup name>> "def!" = ] [ drop first2 env eval-set! ] }
          { [ dup name>> "let*" = ] [ drop first2 env eval-let* ] }
          { [ env eval-ast dup quotation? ] [ [ env eval-ast ] dip with-datastack first ] }
          [ drop "not a fn" throw ]
      } cond ]
    [ env eval-ast ]
    if ;
: PRINT ( maltype -- str ) pr-str ;
: rep ( x -- x ) [ READ repl-env get EVAL PRINT ] [ nip ] recover ;

: main-loop ( -- )
            f repl-bindings <malenv> repl-env set
            [ 1 ]
            [ "user> " readline
              [ 0 exit ] unless*
              rep print flush ]
            while ;

MAIN: main-loop
