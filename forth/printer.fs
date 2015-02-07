require types.fs

: safe-type ( str-addr str-len -- )
    dup 256 > if
        drop 256 type ." ...<lots more>" type
    else
        type
    endif ;

\ === mutable string buffer === /
\ string buffer that maintains an allocation larger than the current
\ string size.  When appending would cause the string size exceed the
\ current allocation, resize is used to double the allocation. The
\ current allocation is not stored anywhere, but computed based on
\ current string size or str-base-size, whichever is larger.
64 constant str-base-size

: new-str ( -- addr length )
  str-base-size allocate throw 0 ;

: round-up ( n -- n )
  2
  begin
    1 lshift 2dup <
  until
  nip ;

: str-append { buf-addr buf-str-len str-addr str-len }
  buf-str-len str-len +
  { new-len }
  new-len str-base-size > if
    buf-str-len new-len xor buf-str-len > if
      buf-addr new-len round-up resize throw
      to buf-addr
    endif
  endif
  str-addr buf-addr buf-str-len + str-len cmove
  buf-addr new-len ;

\ define a-space, to append a space char to a string
bl c,
here constant space-str
: a-space space-str 1 str-append ;

: str-append-char ( buf-addr buf-str-len char -- buf-addr buf-str-len )
    pad ! pad 1 str-append ;

: int>str ( num -- str-addr str-len )
  s>d <# #s #> ;


\ === printer protocol and implementations === /

def-protocol-method pr-buf ( str-addr str-len this -- str-addr str-len )

: pr-str { obj }
    new-str obj pr-buf ;

\ Examples of extending existing protocol methods to existing type
MalDefault
  extend pr-buf
    { this }
    s" #<MalObject" str-append a-space
    this int>str str-append
    s" >" str-append ;;
drop

MalNil
  extend pr-buf
    drop s" nil" str-append ;;
drop

: pr-buf-list-item ( list str-addr str-len -- list str-addr str-len)
    rot dup MalList/cdr @ swap MalList/car @ 2swap rot pr-buf ;

: pr-buf-list ( list str-addr str-len -- str-addr str-len)
    pr-buf-list-item
    begin ( list str-addr str-len )
      2 pick mal-nil <>
    while
      a-space pr-buf-list-item
    repeat
    rot drop ;

MalList
  extend pr-buf
    -rot s" (" str-append ( list str-addr str-len )
    pr-buf-list
    s" )" str-append ;;
drop

MalVector
  extend pr-buf
    MalVector/list @
    -rot s" [" str-append ( list str-addr str-len )
    pr-buf-list
    s" ]" str-append ;;
drop

MalMap
  extend pr-buf
    MalMap/list @
    -rot s" {" str-append ( list str-addr str-len )
    pr-buf-list-item a-space pr-buf-list-item
    begin ( list str-addr str-len )
      2 pick mal-nil <>
    while
      s" , " str-append
      pr-buf-list-item a-space pr-buf-list-item
    repeat
    rot drop
    s" }" str-append ;;
drop

MalInt
  extend pr-buf
    MalInt/int @ int>str str-append ;;
drop

MalSymbol
  extend pr-buf
    dup MalSymbol/sym-addr @
    swap MalSymbol/sym-len @
    str-append ;;
drop

: insert-\ ( str-addr str-len insert-idx -- str-addr str-len )
    -rot 0 str-append-char { addr len }
    dup   dup addr +   dup 1+  ( i i from to )
    rot len swap - cmove> ( i ) \ shift " etc to the right
    addr + [char] \ swap c! \ escape it!
    addr len
    ;

MalString
  extend pr-buf
    dup MalString/str-addr @
    swap MalString/str-len @
    { addr len }

    s\" \"" str-append
    0 ( i )
    begin
        dup len <
    while
        dup addr + c@ ( i char )
        dup [char] " = over [char] \ = or if ( i char )
            drop dup addr len rot insert-\ to len to addr
            1+
        else
            dup 10 = if ( i ) \ newline?
                drop dup addr len rot insert-\ to len to addr
                dup addr + 1+ [char] n swap c!
                1+
            else
                13 = if ( i ) \ return?
                    dup addr len rot insert-\ to len to addr
                    dup addr + 1+ [char] r swap c!
                    1+
                endif
            endif
        endif
        1+
    repeat
    drop addr len str-append
    s\" \"" str-append ;;
drop
