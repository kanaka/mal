! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel hashtables accessors assocs locals math sequences ;
IN: malenv

TUPLE: malenv
{ outer read-only }
{ data hashtable read-only } ;

! set outer to f if top level env

INSTANCE: malenv assoc

C: <malenv> malenv
: new-env ( outer -- malenv ) H{ } clone malenv boa ;

M:: malenv at* ( key assoc -- value/f ? )
    key name>> assoc data>> at*
    [ drop assoc outer>>
      [ key ?of ]
      [ f f ]
      if*
    ]
    unless* ;

M: malenv assoc-size ( assoc -- n )
    [ data>> ] [ outer>> ] bi [ assoc-size ] bi@ + ;

M: malenv >alist ( assoc -- n )
    [ data>> ] [ outer>> ] bi [ >alist ] bi@ append ;

M: malenv set-at ( value key assoc -- )
    [ name>> ] [ data>> ] bi* set-at ;

M: malenv delete-at ( key assoc -- )
    [ name>> ] [ data>> ] bi* delete-at ;

M: malenv clear-assoc ( assoc -- )
    data>> clear-assoc ;

: get-or-throw ( key assoc -- value )
    ?at [ dup name>> "'" dup surround " not found" append throw ] unless ;
