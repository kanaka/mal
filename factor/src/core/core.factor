! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math sequences arrays lists printer locals io strings malenv reader io.files io.encodings.utf8
       fry types combinators.short-circuit vectors hashtables assocs hash-sets sets grouping namespaces accessors
       combinators readline system ;

IN: core

SYMBOL: mal-apply

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
                { "=" [ first2 2dup [ { [ ] [ sequence? ] [ string? not ] } 1&& ] bi@ and [ sequence= ] [ = ] if ] }
                { "<" [ first2 < ] }
                { ">" [ first2 > ] }
                { ">=" [ first2 >= ] }
                { "<=" [ first2 <= ] }
                { "pr-str" [ t " " pr-str-stack ] }
                { "str" [ f "" pr-str-stack ] }
                { "prn" [ t " " pr-str-stack print flush nil ] }
                { "println" [ f " " pr-str-stack print flush nil ] }
                { "read-string" [ first read-str ] }
                { "slurp" [ first utf8 file-contents ] }
                { "cons" [ first2 swap prefix to-array ] }
                { "concat" [ concat to-array ] }
                { "nth" [ first2 swap nth ] }
                { "first" [ first dup empty? [ drop nil ] [ first ] if ] }
                { "rest" [ first dup empty? [ drop { } ] [ rest to-array ] if ] }
                { "throw" [ first throw ] }
                { "apply" [ unclip [ unclip-last append ] dip mal-apply get call( args fn -- maltype ) ] }
                { "map" [ first2 swap '[ 1array _ mal-apply get call( args fn -- maltype ) ] map to-array ] }
                { "nil?" [ first nil? ] }
                { "true?" [ first t = ] }
                { "false?" [ first f = ] }
                { "symbol" [ first <malsymbol> ] }
                { "symbol?" [ first malsymbol? ] }
                { "keyword" [ first dup 1 head "\u00029e" = [ "\u00029e" prepend ] unless ] }
                { "keyword?" [ first { [ string? ] [ 1 head "\u00029e" = ] } 1&& ] }
                { "vector" [ >vector ] }
                { "vector?" [ first vector? ] }
                { "hash-map" [ 2 group parse-hashtable ] }
                { "map?" [ first hashtable? ] }
                { "assoc" [ unclip swap 2 group parse-hashtable assoc-union ] }
                { "dissoc" [ unclip swap >hash-set '[ drop _ in? not ] assoc-filter ] }
                { "get" [ first2 swap dup nil? [ nip ] [ ?at [ drop nil ] unless ] if ] }
                { "contains?" [ first2 swap dup nil? [ nip ] [ ?at nip ] if ] }
                { "keys" [ first keys ] }
                { "vals" [ first values ] }
                { "sequential?" [ first { [ vector? ] [ array? ] } 1|| ] }

                { "readline" [ first readline ] }
                { "meta" [ first dup fn? [ meta>> ] [ drop f ] if [ nil ] unless* ] }
                { "with-meta" [ first2 over fn? [ [ clone ] dip >>meta ] when ] }
                { "atom" [ first <malatom> ] }
                { "atom?" [ first malatom? ] }
                { "deref" [ first val>> ] }
                { "reset!" [ first2 >>val val>> ] }
                { "swap!" [ { [ first ] [ second ] [ 2 tail ] [ first val>> ] } cleave
                            prefix swap mal-apply get call( args fn -- maltype ) >>val val>> ] }
                { "conj" [ unclip swap over array? [ reverse prepend ] [ append ] if ] }
                { "time-ms" [ drop nano-count 1000000 / >integer ] }
             }
