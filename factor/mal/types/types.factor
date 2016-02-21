! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators.short-circuit hashtables
kernel locals mal.env sequences strings ;
IN: mal.types

TUPLE: malsymbol { name string read-only } ;

C: <malsymbol> malsymbol

: symeq? ( string other -- ? )
    dup malsymbol? [ name>> = ] [ 2drop f ] if ;

TUPLE: malfn
    { env malenv read-only }
    { binds sequence read-only }
    { exprs read-only }
    { macro? boolean }
    { meta assoc } ;

: <malfn> ( env binds exprs -- fn )
    f f malfn boa ;

TUPLE: malatom { val } ;

C: <malatom> malatom

TUPLE: malkeyword { name string read-only } ;

C: <malkeyword> malkeyword

DEFER: mal=

: mal-sequence= ( seq1 seq2 -- ? )
    2dup [ length ] bi@ =
    [ [ mal= ] 2all? ] [ 2drop f ] if ;

:: mal-hashtable= ( h1 h2 -- ? )
    h1 assoc-size h2 assoc-size = [
        h1 [| k1 v1 | k1 h2 at* drop v1 mal= ] assoc-all?
    ] [ f ] if ;

: mal= ( obj1 obj2 -- ? )
    2dup [ hashtable? ] bi@ and
    [ mal-hashtable= ] [
        2dup [ { [ ] [ sequence? ] [ string? not ] } 1&& ] bi@ and
        [ mal-sequence= ] [ = ] if
    ] if ;
