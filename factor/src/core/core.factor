! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math sequences arrays lists printer locals io strings malenv reader io.files io.encodings.utf8 ;

IN: core

:: pr-str-stack ( exprs readably? glue -- str )
    exprs [ readably? (pr-str) ] map glue join ;

: to-array ( seq -- array )
    dup array? [ >array ] unless ;

CONSTANT: empty-env T{ malenv f H{ } }

CONSTANT: ns H{ { "+" [ first2 + ] }
                { "-" [ first2 - ] }
                { "*" [ first2 * ] }
                { "/" [ first2 / ] }
                { "list" [ >array ] }
                { "list?" [ first array? ] }
                { "empty?" [ first empty? ] }
                { "count" [ first dup nil? [ drop 0 ] [ length ] if ] }
                { "=" [ first2 2dup [ [ sequence? ] [ string? not ] bi and ] bi@ and [ sequence= ] [ = ] if ] }
                { "<" [ first2 < ] }
                { ">" [ first2 > ] }
                { ">=" [ first2 >= ] }
                { "<=" [ first2 <= ] }
                { "pr-str" [ t " " pr-str-stack ] }
                { "str" [ f "" pr-str-stack ] }
                { "prn" [ t " " pr-str-stack print nil ] }
                { "println" [ f " " pr-str-stack print nil ] }
                { "read-string" [ first read-str ] }
                { "slurp" [ first utf8 file-contents ] }
                { "cons" [ first2 swap prefix to-array ] }
                { "concat" [ concat to-array ] }
                { "nth" [ first2 swap nth ] }
                { "first" [ first dup empty? [ drop nil ] [ first ] if ] }
                { "rest" [ first dup empty? [ drop { } ] [ rest to-array ] if ] }
             }
