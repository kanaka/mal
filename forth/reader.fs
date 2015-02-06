require types.fs
require printer.fs

\ Drop a char off the front of string by advancing the addr and
\ decrementing the length, and fetch next char
: adv-str ( str-addr str-len -- str-addr str-len char )
    swap 1+ swap 1-
    dup 0= if 0 ( eof )
    else over c@ endif ;

: skip-spaces ( str-addr str-len char -- str-addr str-len non-space-char )
    begin
        dup bl =
    while ( str-addr str-len space-char )
        drop adv-str
    repeat ;

: mal-digit? ( char -- flag )
    dup [char] 9 <= if
        [char] 0 >=
    else
        drop 0
    endif ;

: char-in-str? ( char str-addr str-len )
    rot { needle }
    begin ( str-addr str-len )
        adv-str needle = if
            2drop -1 -1 \ success! drop and exit
        else
            dup 0= if
                2drop 0 -1 \ str consumed, char not found.
            else
                0 \ continue
            endif
        endif
    until ;

s\" []{}()'\"`,; " constant non-sym-chars-len constant non-sym-chars
: sym-char? ( char -- flag )
    non-sym-chars non-sym-chars-len char-in-str? 0= ;

defer read-form ( str-addr str-len -- str-addr str-len mal-obj )

: read-int ( str-addr str-len digit-char -- str-addr str-len non-digit-char mal-int )
    0 { int }
    begin ( str-addr str-len digit-char )
        [char] 0 - int 10 * + to int ( str-addr str-len )
        adv-str dup mal-digit? 0= ( str-addr str-len digit-char )
    until
    int MalInt. ;

: read-symbol-str ( str-addr str-len sym-char -- str-addr str-len sym-addr sym-len )
    new-str { sym-addr sym-len }
    begin ( str-addr srt-len sym-char )
        sym-addr sym-len rot str-append-char to sym-len to sym-addr
        adv-str dup sym-char? 0=
    until
    sym-addr sym-len ;

: read-list ( str-addr str-len open-paren-char -- str-addr str-len non-paren-char mal-list )
    \ push objects onto "dictionary" -- maybe not the best stack for this?
    0 { len }
    drop adv-str
    begin ( str-addr str-len char )
        skip-spaces ( str-addr str-len non-space-char )
        dup [char] ) <>
    while ( str-addr str-len non-space-non-paren-char )
        read-form , len 1+ to len
    repeat
    drop adv-str

    \ pop objects out of "dictionary" into MalList
    mal-nil
    len 0 ?do
        0 cell - allot
        here @ swap conj
    loop
    ;

: read-form2 ( str-addr str-len char -- str-addr str-len mal-obj )
    skip-spaces
    dup mal-digit? if read-int else
        dup [char] ( = if read-list else
            read-symbol-str MalSymbol.
        endif
    endif
    ;
' read-form2 is read-form

: read-str ( str-addr str-len - mal-obj )
    over c@ read-form -rot 2drop ;
