! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: regexp strings kernel sequences math.parser types combinators locals prettyprint make lists math
       grouping hashtables ;
IN: reader

CONSTANT: token-regex R/ (~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)~^@]+)/

DEFER: read-form

: read-atom ( str -- maltype )
    {
        { [ dup string>number ] [ string>number ] }
        { [ dup 1 head "\"" = ] [ rest but-last R/ \\"/ "\"" re-replace ] }
        { [ dup 1 head ":" = ]  [ rest "\u00029e" prepend ] }
        { [ dup "false" = ]     [ drop f ] }
        { [ dup "true" = ]      [ drop t ] }
        { [ dup "nil" = ]       [ drop nil ] }
        [ <malsymbol> ]
    } cond ;

:: read-sequence ( seq closer exemplar -- seq maltype )
    seq
    [ [ [ "expected " closer append throw ] when-empty
        dup first closer = ]
      [ read-form , ]
      until
      rest
    ] exemplar make ;

: read-list ( seq -- seq maltype )
    ")" { } read-sequence ;

: read-vector ( seq -- seq maltype )
    "]" V{ } read-sequence ;

: read-hashmap ( seq -- seq maltype )
    "}" V{ } read-sequence
    2 group parse-hashtable ;

: consume-next-into-list ( seq symname -- seq maltype )
    [ rest read-form ] dip <malsymbol> swap { } 2sequence ;

: read-form ( seq -- seq maltype )
    {
        { [ dup first "(" = ] [ rest read-list ] }
        { [ dup first "[" = ] [ rest read-vector ] }
        { [ dup first "{" = ] [ rest read-hashmap ] }
        { [ dup first "'" = ] [ "quote" consume-next-into-list ] }
        { [ dup first "`" = ] [ "quasiquote" consume-next-into-list ] }
        { [ dup first "~" = ] [ "unquote" consume-next-into-list ] }
        { [ dup first "~@" = ] [ "splice-unquote" consume-next-into-list ] }
        { [ dup first "^" = ] [ rest read-form [ read-form ] dip { } 2sequence "with-meta" <malsymbol> prefix ] }
        { [ dup first "@" = ] [ "deref" consume-next-into-list ] }
        [ [ rest ] [ first read-atom ] bi ]
    } cond ;

: tokenize ( str -- seq )
    token-regex all-matching-slices
    [ >string ] map
    [ 1 head ";" = not ] filter ;

: read-str ( str -- maltype )
    tokenize [ " " throw ] [ read-form nip ] if-empty ;

