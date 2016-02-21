! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit fry grouping hash-sets hashtables io
io.encodings.utf8 io.files kernel lists mal.env mal.printer
mal.reader mal.types math namespaces readline sequences sets
system vectors ;
IN: mal.core

SYMBOL: mal-apply

: pr-str-stack ( exprs readably? glue -- str )
    [ '[ _ (pr-str) ] map ] dip join ;

CONSTANT: empty-env T{ malenv f f H{ } }

CONSTANT: ns H{
    { "+" [ first2 + ] }
    { "-" [ first2 - ] }
    { "*" [ first2 * ] }
    { "/" [ first2 / ] }
    { "list" [ >array ] }
    { "list?" [ first array? ] }
    { "empty?" [ first empty? ] }
    { "count" [ first dup nil? [ drop 0 ] [ length ] if ] }
    { "=" [ first2 mal= ] }
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
    { "cons" [ first2 swap prefix { } like ] }
    { "concat" [ concat { } like ] }
    { "nth" [ first2 swap nth ] }
    { "first" [ first dup nil? [ drop nil ] [ [ nil ] [ first ] if-empty ] if ] }
    { "rest" [ first dup nil? [ drop { } ] [ [ { } ] [ rest { } like ] if-empty ] if ] }
    { "throw" [ first throw ] }
    { "apply" [ unclip [ unclip-last append ] dip mal-apply get call( args fn -- maltype ) ] }
    { "map" [ first2 swap '[ 1array _ mal-apply get call( args fn -- maltype ) ] map { } like ] }
    { "nil?" [ first nil? ] }
    { "true?" [ first t = ] }
    { "false?" [ first f = ] }
    { "symbol" [ first <malsymbol> ] }
    { "symbol?" [ first malsymbol? ] }
    { "keyword" [ first <malkeyword> ] }
    { "keyword?" [ first malkeyword? ] }
    { "vector" [ >vector ] }
    { "vector?" [ first vector? ] }
    { "hash-map" [ 2 group parse-hashtable ] }
    { "map?" [ first hashtable? ] }
    { "assoc" [ unclip swap 2 group parse-hashtable assoc-union ] }
    { "dissoc" [ unclip swap >hash-set '[ drop _ in? not ] assoc-filter ] }
    { "get" [ first2 swap dup nil? [ nip ] [ ?at [ drop nil ] unless ] if ] }
    { "contains?" [ first2 swap dup nil? [ nip ] [ at* nip ] if ] }
    { "keys" [ first keys ] }
    { "vals" [ first values ] }
    { "sequential?" [ first { [ vector? ] [ array? ] } 1|| ] }
    { "readline" [ first readline ] }
    { "meta" [ first dup malfn? [ meta>> ] [ drop f ] if [ nil ] unless* ] }
    { "with-meta" [ first2 over malfn? [ [ clone ] dip >>meta ] [ drop ] if ] }
    { "atom" [ first <malatom> ] }
    { "atom?" [ first malatom? ] }
    { "deref" [ first val>> ] }
    { "reset!" [ first2 >>val val>> ] }
    { "swap!" [ { [ first ] [ second ] [ 2 tail ] [ first val>> ] } cleave
                prefix swap mal-apply get call( args fn -- maltype ) >>val val>> ] }
    { "conj" [ unclip swap over array? [ reverse prepend ] [ append ] if ] }
    { "time-ms" [ drop nano-count 1,000,000 /i ] }
}
