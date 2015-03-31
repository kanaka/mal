! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io readline kernel system reader printer continuations  arrays locals assocs sequences
       combinators accessors fry quotations math malenv namespaces grouping hashtables lists
       types splitting core ;

IN: step4_if_fn_do

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
    env params first [ name>> ] map params second <fn> ;

: args-split ( bindlist -- bindlist restbinding/f )
    [ "&" ] split dup length 1 >
    [ first2 first ]
    [ first f ]
    if ;

:: make-bindings ( args bindlist restbinding/f -- bindingshash )
    bindlist
    args bindlist length cut-slice
    [ zip ] dip
    restbinding/f
    [ swap >array 2array suffix ]
    [ drop ]
    if*
    >hashtable ;

: apply ( args fn -- maltype )
    {
        { [ dup fn? ]
          [ [ exprs>> nip ] [ env>> nip ] [ binds>> args-split make-bindings ] 2tri <malenv>
          EVAL ] }
        { [ dup callable? ] [ call( x -- y ) ] }
        [ drop "not a fn" throw ]
    } cond ;

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
          [ prefix [ env EVAL ] map unclip apply ]
      } cond ]
    [ env eval-ast ]
    if ;
: PRINT ( maltype -- str ) pr-str ;
: rep ( x -- x ) [ READ repl-env get EVAL PRINT ] [ nip ] recover ;

: main-loop ( -- )
            [ 1 ]
            [ "user> " readline
              [ 0 exit ] unless*
              rep print flush ]
            while ;


f ns <malenv> repl-env set-global
"(def! not (fn* (a) (if a false true)))" rep drop

MAIN: main-loop
