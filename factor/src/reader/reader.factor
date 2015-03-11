! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: regexp strings kernel sequences math.parser types combinators locals prettyprint make ;
IN: reader

CONSTANT: token-regex R/ (~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]+)/

DEFER: read-form

: read-atom ( str -- maltype )
    {
        { [ dup string>number ] [ string>number ] }
        [ <malsymbol> ]
    } cond ;

: read-list ( seq -- seq maltype )
    [ [ dup first ")" = not ]
      [ read-form , ]
      while
      rest
    ] V{ } make ;

: read-form ( seq -- seq maltype )
    dup first "(" =
    [ rest read-list ]
    [ [ rest ] [ first read-atom ] bi ]
    if ;

: tokenize ( str -- seq )
    token-regex all-matching-slices [ >string ] map ;

: read-str ( str -- maltype )
    tokenize read-form nip ;

