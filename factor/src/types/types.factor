! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: regexp strings kernel sequences math.parser ;
IN: types

TUPLE: malsymbol { name string read-only } ;
C: <malsymbol> malsymbol
