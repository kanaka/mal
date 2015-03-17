! Copyright (C) 2015 Jordan Lewis.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math sequences arrays lists printer locals io strings ;

IN: core

:: pr-str-stack ( printer-quot glue -- str )
    datastack printer-quot map glue join ; inline

CONSTANT: ns H{ { "+" [ + ] }
                { "-" [ - ] }
                { "*" [ * ] }
                { "/" [ / ] }
                { "list" [ datastack >array ] }
                { "list?" [ array? ] }
                { "empty?" [ empty? ] }
                { "count" [ dup nil? [ drop 0 ] [ length ] if ] }
                { "=" [ 2dup [ [ sequence? ] [ string? not ] bi and ] bi@ and [ sequence= ] [ = ] if ] }
                { "<" [ < ] }
                { ">" [ > ] }
                { ">=" [ >= ] }
                { "<=" [ <= ] }
                { "pr-str" [ [ t (pr-str) ] " " pr-str-stack ] }
                { "str" [ [ f (pr-str) ] "" pr-str-stack ] }
                { "prn" [ [ t (pr-str) ] " " pr-str-stack print nil ] }
                { "println" [ [ f (pr-str) ] " " pr-str-stack print nil ] }
             }
