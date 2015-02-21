require types.fs
require printer.fs

\ Drop a char off the front of string by advancing the addr and
\ decrementing the length, and fetch next char
: adv-str ( str-addr str-len -- str-addr str-len char )
    swap 1+ swap 1-
    dup 0= if 0 ( eof )
    else over c@ endif ;

: mal-digit? ( char -- flag )
    dup [char] 9 <= if
        [char] 0 >=
    else
        drop 0
    endif ;

: char-in-str? ( char str-addr str-len )
    rot { needle }
    false -rot
    over + swap ?do
        i c@ needle = if drop true leave endif
    loop ;

: sym-char? ( char -- flag )
    s\" \n\r\t\000[]{}()'\"`,; " char-in-str? 0= ;

: skip-spaces ( str-addr str-len char -- str-addr str-len non-space-char )
    begin
        begin
            dup s\" \n\r\t, " char-in-str?
        while ( str-addr str-len space-char )
            drop adv-str
        repeat
        dup [char] ; = if
            drop
            begin
                adv-str s\" \n\r\000" char-in-str?
            until
            adv-str false
        else
            true
        endif
    until ;

defer read-form ( str-addr str-len -- str-addr str-len mal-obj )

: read-int ( str-addr str-len digit-char -- str-addr str-len non-digit-char mal-int )
    0 { int }
    begin ( str-addr str-len digit-char )
        [char] 0 - int 10 * + to int ( str-addr str-len )
        adv-str dup mal-digit? 0= ( str-addr str-len digit-char )
    until
    int MalInt. ;

: read-symbol-str ( str-addr str-len sym-char -- str-addr str-len char sym-addr sym-len )
    new-str { sym-addr sym-len }
    begin ( str-addr str-len sym-char )
        sym-addr sym-len rot str-append-char to sym-len to sym-addr
        adv-str dup sym-char? 0=
    until
    sym-addr sym-len ;

: read-string-literal ( in-addr in-len quote-char -- in-addr in-len mal-string )
    new-str { out-addr out-len }
    drop \ drop leading quote
    begin ( in-addr in-len )
        adv-str over 0= if
            2drop 0 0 s\" expected '\"', got EOF" ...throw-str
        endif
        dup [char] " <>
    while
        dup [char] \ = if
            drop adv-str
            dup [char] n = if drop 10 endif
            dup [char] r = if drop 13 endif
        endif
        out-addr out-len rot str-append-char to out-len to out-addr
    repeat
    drop adv-str \ skip trailing quote
    out-addr out-len MalString. ;

: read-list ( str-addr str-len open-paren-char close-paren-char
                  -- str-addr str-len non-paren-char mal-list )
    here { close-char old-here }
    drop adv-str
    begin ( str-addr str-len char )
        skip-spaces ( str-addr str-len non-space-char )
        over 0= if
            drop 2drop 0 0 s" ', got EOF"
            close-char pad ! pad 1
            s" expected '" ...throw-str
        endif
        dup close-char <>
    while ( str-addr str-len non-space-non-paren-char )
            read-form ,
    repeat
    drop adv-str
    old-here here>MalList ;

s" deref"          MalSymbol. constant deref-sym
s" quote"          MalSymbol. constant quote-sym
s" quasiquote"     MalSymbol. constant quasiquote-sym
s" splice-unquote" MalSymbol. constant splice-unquote-sym
s" unquote"        MalSymbol. constant unquote-sym

: read-wrapped ( buf-addr buf-len quote-char sym-addr sym-len -- buf-addr buf-len char mal-list )
    here { old-here }
    , ( buf-addr buf-len char )
    read-form , ( buf-addr buf-len char )
    old-here here>MalList ;

: read-form2 ( str-addr str-len char -- str-addr str-len char mal-obj )
    skip-spaces
    dup mal-digit? if read-int else
    dup [char] ( = if [char] ) read-list else
    dup [char] [ = if [char] ] read-list MalVector new tuck MalVector/list ! else
    dup [char] { = if [char] } read-list MalMap new tuck MalMap/list ! else
    dup [char] " = if read-string-literal else
    dup [char] : = if drop adv-str read-symbol-str MalKeyword. else
    dup [char] @ = if drop adv-str deref-sym read-wrapped else
    dup [char] ' = if drop adv-str quote-sym read-wrapped else
    dup [char] ` = if drop adv-str quasiquote-sym read-wrapped else
    dup [char] ~ = if
        drop adv-str
        dup [char] @ = if drop adv-str splice-unquote-sym read-wrapped
        else unquote-sym read-wrapped
        endif
    else
    dup [char] ^ = if
        drop adv-str
        read-form { meta } read-form { obj }
        meta mal-nil conj
        obj swap conj
        s" with-meta" MalSymbol. swap conj
    else
        read-symbol-str
        2dup s" true" str= if 2drop mal-true
        else 2dup s" false" str= if 2drop mal-false
        else 2dup s" nil" str= if 2drop mal-nil
        else
          MalSymbol.
    endif endif endif endif endif endif endif endif endif endif endif endif endif endif ;
' read-form2 is read-form

: read-str ( str-addr str-len - mal-obj )
    over c@ read-form { obj } drop 2drop obj ;
