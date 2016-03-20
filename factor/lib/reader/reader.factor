! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: arrays combinators grouping hashtables kernel lists
locals make lib.types math.parser regexp sequences splitting ;
IN: lib.reader

CONSTANT: token-regex R/ (~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)~^@]+)/

DEFER: read-form

: (read-atom) ( str -- maltype )
    {
        { [ dup first CHAR: " = ] [ rest but-last "\\\"" "\"" replace
                                                  "\\n"  "\n" replace
                                                  "\\\\" "\\" replace ] }
        { [ dup first CHAR: : = ] [ rest <malkeyword> ] }
        { [ dup "false" = ]       [ drop f ] }
        { [ dup "true" = ]        [ drop t ] }
        { [ dup "nil" = ]         [ drop nil ] }
        [ <malsymbol> ]
    } cond ;

: read-atom ( str -- maltype )
    dup string>number [ nip ] [ (read-atom) ] if* ;

:: read-sequence ( seq closer exemplar -- seq maltype )
    seq [
        [
            [ "expected " closer append throw ]
            [ dup first closer = ] if-empty
        ] [
            read-form ,
        ] until rest
    ] exemplar make ;

: read-list ( seq -- seq maltype )
    ")" { } read-sequence ;

: read-vector ( seq -- seq maltype )
    "]" V{ } read-sequence ;

: read-hashmap ( seq -- seq maltype )
    "}" V{ } read-sequence 2 group parse-hashtable ;

: consume-next-into-list ( seq symname -- seq maltype )
    [ read-form ] dip <malsymbol> swap 2array ;

: read-form ( seq -- seq maltype )
    unclip {
        { "(" [ read-list ] }
        { "[" [ read-vector ] }
        { "{" [ read-hashmap ] }
        { "'" [ "quote" consume-next-into-list ] }
        { "`" [ "quasiquote" consume-next-into-list ] }
        { "~" [ "unquote" consume-next-into-list ] }
        { "~@" [ "splice-unquote" consume-next-into-list ] }
        { "^" [ read-form [ read-form ] dip 2array "with-meta" <malsymbol> prefix ] }
        { "@" [ "deref" consume-next-into-list ] }
        [ read-atom ]
    } case ;

: tokenize ( str -- seq )
    token-regex all-matching-subseqs
    [ first CHAR: ; = not ] filter ;

: read-str ( str -- maltype )
    tokenize [ " " throw ] [ read-form nip ] if-empty ;
