! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: types vectors math math.parser kernel accessors sequences combinators strings arrays lists
       hashtables assocs combinators.short-circuit regexp quotations locals summary ;
IN: printer

:: pr-str-str ( str readably?  -- str )
    str dup { [ empty? not ] [ 1 head "\u00029e" = ] } 1&&
    [ rest ":" prepend ]
    [ readably? [ R/ \/ "\\\\" re-replace
                  R/ "/ """\\"""" re-replace
                  "\"" dup surround ] when ]
    if ;

:: (pr-str) ( maltype readably? -- str )
    maltype
    {
        { [ dup malsymbol? ] [ name>> ] }
        { [ dup number? ]    [ number>string ] }
        { [ dup string? ]    [ readably? pr-str-str ] }
        { [ dup array? ]     [ [ readably? (pr-str) ] map " " join "(" ")" surround ] }
        { [ dup vector? ]    [ [ readably? (pr-str) ] map " " join "[" "]" surround ] }
        { [ dup hashtable? ] [ unzip
                               [ [ readably? (pr-str) ] bi@ " " glue ] 2map " " join
                               "{" "}" surround ] }
        { [ dup fn? ]        [ drop "#<fn>" ] }
        { [ dup t = ]        [ drop "true" ] }
        { [ dup f = ]        [ drop "false" ] }
        { [ dup nil = ]      [ drop "nil" ] }
        { [ dup malatom? ]   [ val>> readably? (pr-str) "(atom " ")" surround ] }
        [ summary ]
    } cond ;

: pr-str ( maltype -- str )
    t (pr-str) ;
