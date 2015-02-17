require types.fs

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

: int>str ( num -- str-addr str-len )
  s>d <# #s #> ;


\ === printer protocol and implementations === /

def-protocol-method pr-buf ( readably? str-addr str-len this -- str-addr str-len )
def-protocol-method pr-seq-buf ( readably? str-addr str-len this -- str-addr str-len )
def-protocol-method pr-pairs-buf ( readably? str-addr str-len this -- str-addr str-len )

: pr-str { obj }
    true new-str obj pr-buf rot drop ;

\ Examples of extending existing protocol methods to existing type
MalDefault
  extend pr-buf
    { this }
    s" #<" str-append
    this mal-type @ type-name str-append
    a-space
    this int>str str-append
    s" >" str-append ;;
drop

MalNil   extend pr-buf drop s" nil"   str-append ;; drop
MalTrue  extend pr-buf drop s" true"  str-append ;; drop
MalFalse extend pr-buf drop s" false" str-append ;; drop

MalList
  extend pr-buf
    -rot s" (" str-append ( list str-addr str-len )
    rot pr-seq-buf
    s" )" str-append ;;
  extend pr-seq-buf { list }
    list MalList/count @ 0 > if
        list MalList/start @ { start }
        start @ pr-buf
        list MalList/count @ 1 ?do
            a-space
            start i cells + @ pr-buf
        loop
    endif ;;
  extend pr-pairs-buf { list }
    list MalList/start @ { start }
    start @ pr-buf a-space start cell+ @ pr-buf
    list MalList/count @ 2 / 1 ?do
        s" , " str-append
        a-space
        start i 2 * cells + @ pr-buf a-space
        start i 2 * 1+ cells + @ pr-buf
    loop ;;
drop

MalVector
  extend pr-buf
    MalVector/list @
    -rot s" [" str-append ( list str-addr str-len )
    rot pr-seq-buf
    s" ]" str-append ;;
drop

MalMap
  extend pr-buf
    MalMap/list @
    -rot s" {" str-append ( list str-addr str-len )
    rot pr-pairs-buf
    s" }" str-append ;;
drop

MalInt
  extend pr-buf
    MalInt/int @ int>str str-append ;;
drop

MalSymbol
  extend pr-buf
    unpack-sym str-append ;;
drop

MalKeyword
  extend pr-buf { kw }
    s" :" str-append
    kw unpack-keyword str-append ;;
drop

: escape-str { addr len }
    s\" \"" str-append
    addr len +  addr  ?do
        i c@ case
            [char] " of s\" \\\"" str-append endof
            [char] \ of s\" \\\\" str-append endof
            10 of s\" \\n" str-append endof
            13 of s\" \\r" str-append endof
            -rot i 1 str-append rot
        endcase
    loop
    s\" \"" str-append ;

MalString
  extend pr-buf
    dup MalString/str-addr @
    swap MalString/str-len @
    4 pick if
        escape-str
    else
        str-append
    endif ;;
drop
