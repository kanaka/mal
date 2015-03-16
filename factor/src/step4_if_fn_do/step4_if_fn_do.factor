! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io readline kernel system reader printer continuations  arrays locals assocs sequences
       combinators accessors fry quotations math malenv namespaces grouping hashtables lists
       types ;

IN: step4_if_fn_do

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

:: eval-if ( params env -- maltype )
    {
        { [ params first env EVAL { f +nil+ } index not ] ! condition is true
          [ params second env EVAL ] }
        { [ params length 2 > ] [ params third env EVAL ] }
        [ nil ]
    } cond ;

:: eval-fn* ( params env -- maltype )
    [ datastack params first [ name>> ] map [ length tail* ] keep swap zip >hashtable
      env swap <malenv>
      params second swap
      EVAL ] ;

: READ ( str -- maltype ) read-str ;
:: EVAL ( maltype env -- maltype )
    maltype dup array?
    [ unclip
      {
          { [ "def!" over symeq? ] [ drop first2 env eval-set! ] }
          { [ "let*" over symeq? ] [ drop first2 env eval-let* ] }
          { [ "do" over symeq? ]   [ drop env eval-ast last ] }
          { [ "if" over symeq? ]   [ drop env eval-if ] }
          { [ "fn*" over symeq? ]  [ drop env eval-fn* ] }
          { [ env EVAL dup callable? ] [ [ env eval-ast ] dip  with-datastack last ] }
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

