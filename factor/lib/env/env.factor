! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs formatting hashtables kernel math
sequences typed ;
IN: lib.env

TUPLE: malenv
{ outer read-only }
{ data hashtable read-only } ;

! set outer to f if top level env

C: <malenv> malenv

: new-env ( outer -- malenv ) H{ } clone malenv boa ;

TYPED: env-find ( key malenv: malenv -- value/f ? )
    2dup [ name>> ] [ data>> ] bi* at* [
        [ 2drop ] 2dip
    ] [
        drop outer>> [ env-find ] [ drop f f ] if*
    ] if* ;

TYPED: env-set ( value key malenv: malenv -- )
    [ name>> ] [ data>> ] bi* set-at ;

: env-get ( key assoc -- value )
    dupd env-find [
        nip
    ] [
        drop name>> "'%s' not found" sprintf throw
    ] if ;
