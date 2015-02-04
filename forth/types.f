\ === classic lisp list === /
: cons { addr val -- new-list-address }
  2 cells allocate throw
  dup addr val rot 2! ;

: cdr ( addr -- next-addr )
  cell+ @ ;

: int-pr ( num -- )
  s>d <# #s #> type ;

: prn ( list-address -- )
  ." (" 2@ int-pr
  begin
    space 2@ int-pr
    dup 0=
  until
  .\" )\n" ;

0 1 cons 2 cons 3 cons 4 cons
prn


\ === mutable vector === /
\ Singly-linked list, with an "object" pair that points to both ends.
\ This allows fast append and fast iteration from beginning to end,
\ like a vector.  ...but buys simplicity with mutability
: new-mutvec ( -- mutvec-addr )
  2 cells allocate throw
  dup 0 0 rot 2! ;

: mutvec-append { mutvec-addr value -- }
  2 cells allocate throw    \ new pair
  dup nil value rot 2!      \ put value in new pair
  dup mutvec-addr @
    ?dup 0= if mutvec-addr endif
    cell+ !                 \ update old tail
  mutvec-addr !             \ update object
  ;

new-mutvec
dup 5 mutvec-append
dup 4 mutvec-append
dup 3 mutvec-append
dup 2 mutvec-append
cdr prn


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


bl c,
here constant space-str
: a-space space-str 1 str-append ;

new-str
s" hello there" str-append a-space
s" is this getting ...." str-append a-space
s\" interesting yet?\n" str-append
type

\ A rewrite of the list-printer above, but now using string buffer:
: int-pr-str2 ( num -- str-addr str-len )
  s>d <# #s #> ;

: pr-str2 ( strbuf str-len list-address -- )
  -rot s" (" str-append rot
  2@ 2swap rot int-pr-str2 str-append
  begin
    a-space
    rot 2@ 2swap rot int-pr-str2 str-append
    2 pick 0=
  until
  s" )" str-append rot drop ;

new-str
0 1 cons 2 cons 3 cons 4 cons
pr-str2 type
cr

: test=
  2dup = if
    2drop
  else
    cr ." assert failed on line " sourceline# .
    swap cr ." | got " . cr ." | expected " . cr
  endif ;

\ new-class

struct
  cell% 2 * field MalTypeType-struct
  cell% field MalTypeType-methods
end-struct MalTypeType%

struct
  cell% field mal-type
  \ cell% field ref-count \ Ha, right.
end-struct MalType%

: new ( MalTypeType -- obj )
  dup MalTypeType-struct 2@ %allocate throw ( MalTypeType obj ) \ create struct
  dup -rot mal-type !                       ( obj ) \ set struct's type pointer to this type
  ;

: deftype* ( struct-align struct-len -- MalTypeType )
  MalTypeType% %allot                ( s-a s-l MalTypeType )
  dup 2swap rot                      ( MalTypeType s-a s-l MalTypeType )
  MalTypeType-struct 2!              ( MalTypeType ) \ store struct info
  dup MalTypeType-methods nil swap ! ( MalTypeType ) \ init methods to nil
  ;

\ Example:

MalType%
  cell% field obj-list/car
  cell% field obj-list/cdr
deftype* constant ObjList

ObjList new
ObjList new
= 0 test=

ObjList new dup obj-list/car 5 swap ! obj-list/car @   5 test=

\ search a sorted array for key, returning the index of where it was
\ found. If key is not in the array, return the index where it would
\ be if added.
: aarray-find { aa-length aa-addr key -- index }
  0 aa-length ( start end )
  begin
    \ cr 2dup . .
    2dup + 2 / dup ( start end middle middle )
    cells aa-addr + @ ( start end middle mid-val )
    dup key < if
      drop rot ( end middle start )
      2dup = if
        2drop dup ( end end )
      else
        drop swap ( middle end )
      endif
    else
      key > if ( start end middle )
        swap drop ( start middle )
      else
        -rot 2drop dup ( middle middle )
      endif
    endif
  2dup = until
  drop
  ;

create zaa 2 , 6 , 7 , 10 , 15 , 80 , 81 ,

7 zaa 2 aarray-find   0 test=
7 zaa 6 aarray-find   1 test=
7 zaa 10 aarray-find  3 test=
7 zaa 81 aarray-find  6 test=
7 zaa 12 aarray-find  4 test=
7 zaa 8  aarray-find  3 test=
7 zaa 100 aarray-find 7 test=
7 zaa 1 aarray-find   0 test=

\ manual protocol method

0 value method-count

: pr-str ( ?? obj -- ?? )
  dup mal-type @ MalTypeType-methods @ ( obj methods )
  [ method-count ] literal aarray-find ( obj xt )
  execute ;


(
  method-count 1+ to method-count

protocol IPrintable
  method% pr-str
end-protocol
)

(
ObjList IPrintable extend

  ' pr-str :noname drop s" <unprintable>" ; extend-method*

  extend-method pr-str
    drop s" <unprintable>" ;
end-extend
)

\ new-obj

\ new-instance


\ maybe useful for debugging?
: p dup . ;
: @p dup @ dup . ;

(

create buff 128 allot

." user> "

buff 128 stdin read-line throw

buff c@ .
buff 5 + c@ .

S" Hello" dup . type


bye

)

cr ." Done loading" cr
