: safe-type ( str-addr str-len -- )
    dup 256 > if
        drop 256 type ." ...<lots more>"
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
  new-len str-base-size >= if
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

\ from gforth docs, there named 'my-.'
: int>str ( num -- str-addr str-len )
    \ handling negatives.. behaves like Standard .
    s>d            \ convert to signed double
    swap over dabs \ leave sign byte followed by unsigned double
    <<#            \ start conversion
    #s             \ convert all digits
    rot sign       \ get at sign byte, append "-" if needed
    #>             \ complete conversion
    #>> ;          \ release hold area

defer MalString.

: ...str
    new-str
    begin
        2swap
        over 0 <>
    while
        str-append
    repeat
    2drop MalString. ;

nil value exception-object

: ...throw-str
    ...str to exception-object
    1 throw ;
