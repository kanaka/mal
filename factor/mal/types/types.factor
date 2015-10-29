! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators.short-circuit kernel mal.env
sequences strings ;
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

: mal= ( obj1 obj2 -- ? )
    2dup [ { [ ] [ sequence? ] [ string? not ] } 1&& ] bi@ and
    [ sequence= ] [ = ] if ;
