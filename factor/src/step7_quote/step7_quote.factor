! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: io readline kernel system reader printer continuations  arrays locals assocs sequences
       combinators accessors fry quotations math malenv namespaces grouping hashtables lists
       types splitting core command-line combinators.short-circuit prettyprint ;

IN: step7_quote

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

:: eval-let* ( bindings body env -- maltype env )
    body bindings 2 group env new-env
    [| env pair | pair first2 env EVAL swap env ?set-at ]
    reduce ;

:: eval-do ( exprs env -- lastform env )
    exprs empty?
    [ { } f ]
    [ exprs unclip-last env swap [ eval-ast ] dip nip env ]
    if ;

:: eval-if ( params env -- maltype env/f )
    {
        { [ params first env EVAL { f +nil+ } index not ] ! condition is true
          [ params second env ] }
        { [ params length 2 > ] [ params third env ] }
        [ nil f ]
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

: apply ( args fn -- maltype newenv/f )
    {
        { [ dup fn? ]
          [ [ exprs>> nip ] [ env>> nip ] [ binds>> args-split make-bindings ] 2tri <malenv> ] }
        { [ dup callable? ] [ call( x -- y ) f ] }
        [ drop "not a fn" throw ]
    } cond ;

: is-pair? ( maltype -- bool )
    { [ sequence? ] [ empty? not ] } 1&& ;

: quasiquote ( maltype -- maltype )
    {
        { [ dup is-pair? not ] [ [ "quote" <malsymbol> ] dip 2array ] }
        { [ "unquote" over first symeq? ] [ second ] }
        { [ dup first { [ is-pair? ] [ first "splice-unquote" swap symeq? ] } 1&& ]
          [ [ "concat" <malsymbol> ] dip unclip second swap quasiquote 3array ] }
        [ "cons" <malsymbol> swap unclip quasiquote swap quasiquote 3array ]
    } cond ;

: READ ( str -- maltype ) read-str ;
: EVAL ( maltype env -- maltype )
    [ dup ]
    [ over array?
      [ [ unclip ] dip swap ! rest env first
        {
            { [ "def!" over symeq? ]  [ drop [ first2 ] dip eval-set! f ] }
            { [ "let*" over symeq? ]  [ drop [ first2 ] dip eval-let* ] }
            { [ "do" over symeq? ]    [ drop eval-do ] }
            { [ "if" over symeq? ]    [ drop eval-if ] }
            { [ "fn*" over symeq? ]   [ drop eval-fn* f ] }
            { [ "quote" over symeq? ] [ 2drop first f ] }
            { [ "quasiquote" over symeq? ] [ drop [ first quasiquote ] dip ] }
            [ swap [ prefix ] dip '[ _ EVAL ] map unclip apply ]
        } cond ]
      [ eval-ast f ]
      if ]
    while drop ;

: PRINT ( maltype -- str ) pr-str ;
: rep ( x -- x ) [ READ repl-env get EVAL PRINT ] [ nip pr-str ] recover ;

: main-loop ( -- )
            [ 1 ]
            [ "user> " readline
              [ 0 exit ] unless*
              rep print flush ]
            while ;

f ns <malenv> repl-env set-global

[ first repl-env get EVAL ] "eval" repl-env get data>> set-at
command-line get "*ARGV*" repl-env get data>> set-at

"(def! not (fn* (a) (if a false true)))" rep drop
"(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" rep drop

MAIN: main-loop
