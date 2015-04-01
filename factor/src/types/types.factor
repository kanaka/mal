! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: regexp strings kernel sequences math.parser accessors malenv assocs ;
IN: types

TUPLE: malsymbol { name string read-only } ;
C: <malsymbol> malsymbol

: symeq? ( string other -- ? )
    dup malsymbol? [ name>> = ] [ 2drop f ] if ;

TUPLE: fn { env malenv read-only }
          { binds sequence read-only }
          { exprs read-only }
          { is-macro boolean }
          { meta assoc } ;

C: (<fn>) fn

: <fn> ( env binds exprs -- fn )
    f f (<fn>) ;

TUPLE: malatom { val } ;

C: <malatom> malatom
