! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs fry hashtables kernel lists
mal.types math math.parser sequences splitting strings summary
vectors ;
IN: mal.printer

GENERIC# (pr-str) 1 ( maltype readably? -- str )
M: object (pr-str) drop summary ;
M: malatom (pr-str) [ val>> ] dip (pr-str) "(atom " ")" surround ;
M: malfn (pr-str) 2drop "#<fn>" ;
M: malkeyword (pr-str) drop name>> ":" prepend ;
M: malsymbol (pr-str) drop name>> ;
M: number (pr-str) drop number>string ;
M: string (pr-str)
    [
        "\\" "\\\\" replace
        "\"" "\\\"" replace
        "\n" "\\n" replace
        "\"" dup surround
    ] when ;
M: array (pr-str) '[ _ (pr-str) ] map " " join "(" ")" surround ;
M: vector (pr-str) '[ _ (pr-str) ] map " " join "[" "]" surround ;
M: hashtable (pr-str)
    [ unzip ] dip '[ [ _ (pr-str) ] bi@ " " glue ] 2map
    " " join "{" "}" surround ;
M: t (pr-str) 2drop "true" ;
M: f (pr-str) 2drop "false" ;
M: +nil+ (pr-str) 2drop "nil" ;

: pr-str ( maltype -- str )
    t (pr-str) ;
