! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: types vectors math math.parser kernel accessors sequences combinators strings arrays lists
       hashtables assocs combinators.short-circuit regexp quotations ;
IN: printer

: pr-str-str ( str -- str )
    dup { [ empty? not ] [ 1 head "\u00029e" = ] } 1&&
    [ rest ":" prepend ]
    [ R/ "/ "\\\"" re-replace "\"" dup surround ]
    if ;

: pr-str ( maltype -- str )
    {
        { [ dup malsymbol? ] [ name>> ] }
        { [ dup number? ]    [ number>string ] }
        { [ dup string? ]    [ pr-str-str ] }
        { [ dup array? ]     [ [ pr-str ] map " " join "(" ")" surround ] }
        { [ dup vector? ]    [ [ pr-str ] map " " join "[" "]" surround ] }
        { [ dup hashtable? ] [ unzip
                               [ [ pr-str ] bi@ " " glue ] [ " " glue ] 2map-reduce
                               "{" "}" surround ] }
        { [ dup callable? ]  [ drop "#<fn>" ] }
        { [ dup t = ]        [ drop "true" ] }
        { [ dup f = ]        [ drop "false" ] }
        { [ dup nil = ]      [ drop "nil" ] }
    } cond ;
