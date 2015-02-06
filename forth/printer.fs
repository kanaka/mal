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
  swap drop ;

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
    pad ! pad 1 str-append ; \ refactoring str-append could perhaps make this faster

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

MalList
  extend pr-buf
    -rot s" (" str-append ( list str-addr str-len )
    rot dup MalList/cdr @ swap MalList/car @ 2swap rot pr-buf
    begin ( list str-addr str-len )
      2 pick mal-nil <>
    while
      a-space
      rot dup MalList/cdr @ swap MalList/car @ 2swap rot pr-buf
    repeat
    s" )" str-append rot drop ;;
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
