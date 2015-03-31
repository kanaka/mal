! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: regexp strings kernel sequences math.parser accessors malenv ;
IN: types

TUPLE: malsymbol { name string read-only } ;
C: <malsymbol> malsymbol

: symeq? ( string other -- ? )
    dup malsymbol? [ name>> = ] [ 2drop f ] if ;

TUPLE: fn { env malenv read-only }
          { binds sequence read-only }
          { exprs read-only } ;

C: <fn> fn
