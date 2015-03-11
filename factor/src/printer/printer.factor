! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: types vectors math math.parser kernel accessors sequences combinators ;
IN: printer

: pr-str ( maltype -- str )
    {
        { [ dup malsymbol? ] [ name>> ] }
        { [ dup number? ]    [ number>string ] }
        { [ dup vector? ]    [ [ pr-str ] [ " " glue ] map-reduce
                               "(" ")" surround ] }
    } cond ;
