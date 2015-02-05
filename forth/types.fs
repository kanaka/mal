\ === tiny framework for inline tests === /
: test=
  2dup = if
    2drop
  else
    cr ." assert failed on line " sourceline# .
    swap cr ." | got " . cr ." | expected " . cr
  endif ;

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

\ === deftype* -- protocol-enabled structs === /
\ Each type has MalTypeType% struct allocated on the stack, with
\ mutable fields pointing to all class-shared resources, specifically
\ the data needed to allocate new instances, and the table of protocol
\ methods that have been extended to the type.
\ Use 'deftype*' to define a new type, and 'new' to create new
\ instances of that type.

struct
  cell% field mal-type
  \ cell% field ref-count \ Ha, right.
end-struct MalType%

struct
  cell% 2 * field MalTypeType-struct
  cell% field MalTypeType-methods
  cell% field MalTypeType-method-keys
  cell% field MalTypeType-method-vals
end-struct MalTypeType%

: new ( MalTypeType -- obj )
  dup MalTypeType-struct 2@ %allocate throw ( MalTypeType obj ) \ create struct
  dup -rot mal-type !                       ( obj ) \ set struct's type pointer to this type
  ;

: deftype* ( struct-align struct-len -- MalTypeType )
  MalTypeType% %allot                      ( s-a s-l MalTypeType )
  dup 2swap rot                            ( MalTypeType s-a s-l MalTypeType )
  MalTypeType-struct 2!                    ( MalTypeType ) \ store struct info
  dup MalTypeType-methods     0   swap !   ( MalTypeType )
  dup MalTypeType-method-keys nil swap !   ( MalTypeType )
  dup MalTypeType-method-vals nil swap !   ( MalTypeType )
  ;

MalType% deftype* constant MalDefault

\ Example and tests

MalType%
  cell% field obj-list/car
  cell% field obj-list/cdr
deftype* constant ObjList

ObjList new
ObjList new
= 0 test=

ObjList new dup obj-list/car 5 swap ! obj-list/car @   5 test=

\ === sorted-array === /
\ Here are a few utility functions useful for creating and maintaining
\ the deftype* method tables. The keys array is kept in sorted order,
\ and the methods array is maintained in parallel so that an index into
\ one corresponds to an index in the other.

\ Search a sorted array for key, returning the index of where it was
\ found. If key is not in the array, return the index where it would
\ be if added.
: array-find { a-length a-addr key -- index found? }
  0 a-length           ( start end )
  begin
    \ cr 2dup . .
    2dup + 2 / dup     ( start end middle middle )
    cells a-addr + @   ( start end middle mid-val )
    dup key < if
      drop rot         ( end middle start )
      2dup = if
        2drop dup      ( end end )
      else
        drop swap      ( middle end )
      endif
    else
      key > if         ( start end middle )
        swap drop      ( start middle )
      else
        -rot 2drop dup ( middle middle )
      endif
    endif
  2dup = until
  cells a-addr + @ key =
  ;

\ Create a new array, one cell in length, initialized the provided value
: new-array { value -- array }
  cell allocate throw value over ! ;

\ Resize a heap-allocated array to be one cell longer, inserting value
\ at idx, and shifting the tail of the array as necessary. Returns the
\ (possibly new) array address
: array-insert { old-array-length old-array idx value -- array }
  old-array old-array-length 1+ cells resize throw
  { a }
  a idx cells +   dup cell+   old-array-length idx - cells   cmove>
  value a idx cells + !
  a
  ;

\ array function tests
create za 2 , 6 , 7 , 10 , 15 , 80 , 81 ,

7 za 2 array-find   -1 test= 0 test=
7 za 6 array-find   -1 test= 1 test=
7 za 10 array-find  -1 test= 3 test=
7 za 81 array-find  -1 test= 6 test=
7 za 12 array-find   0 test= 4 test=
7 za 8  array-find   0 test= 3 test=
7 za 100 array-find  0 test= 7 test=
7 za 1 array-find    0 test= 0 test=

10 new-array
1 swap 0  5 array-insert
2 swap 1  7 array-insert
3 swap 3 12 array-insert
4 swap 4 15 array-insert
5 swap 5 20 array-insert

dup 0 cells + @   5 test=
dup 1 cells + @   7 test=
dup 2 cells + @  10 test=
dup 3 cells + @  12 test=
dup 4 cells + @  15 test=
dup 5 cells + @  20 test=


\ === protocol methods === /

\ Used by protocol methods to find the appropriate implementation of
\ themselves for the given object, and then execute that implementation.
: execute-method { obj pxt -- }
  obj mal-type @ dup MalTypeType-methods 2@ swap ( type methods method-keys )
  dup 0= if \ No protocols extended to this type; check for a default
    2drop drop MalDefault MalTypeType-methods 2@ swap
  endif
  dup 0= if ." No protocols extended to this type or MalDefault" 1 throw endif

  pxt array-find ( type idx found? )
  dup 0= if \ No implementation found for this method; check for a default
    2drop MalDefault MalTypeType-methods 2@ swap
    dup 0= if ." No implementation found for this method, and no protocols extended to MalDefault" 1 throw endif
    pxt array-find ( type idx found? )
  endif
  0= if ." No implementation found" 1 throw endif

  cells swap MalTypeType-method-vals @ + @ ( xt )
  obj swap execute
  ;

\ Extend a type with a protocol method. This mutates the MalTypeType
\ object that represents the MalType being extended.
: extend-method* { type pxt ixt -- type }
  type MalTypeType-methods 2@ swap ( methods method-keys )
  dup 0= if \ no protocols extended to this type
    2drop
    1 type MalTypeType-methods !
    pxt new-array type MalTypeType-method-keys !
    ixt new-array type MalTypeType-method-vals !
  else
    pxt array-find { idx found? }
    found? if \ overwrite
      ." Warning: overwriting protocol method implementation"
      type MalTypeType-method-vals @ idx cells + ixt !
    else \ resize
      type MalTypeType-methods dup @ 1+ dup rot ! ( new-count )
      1- dup type MalTypeType-method-keys @ idx pxt array-insert ( old-count new-array )
        type MalTypeType-method-keys ! ( old-count )
      \ cr ." before: " ObjList MalTypeType-method-vals @ @ . cr
      type MalTypeType-method-vals @ idx ixt array-insert ( new-array )
        type MalTypeType-method-vals !
      \ cr ." after: " ObjList MalTypeType-method-vals @ @ . cr
    endif
  endif
  type
  ;

\ Examples of making new protocol methods (without a protocol to group them yet!)
: pr-str [ latestxt ] literal execute-method ;
: conj   [ latestxt ] literal execute-method ;

\ Examples of extending existing protocol methods to existing type
MalDefault ' pr-str :noname s" #<MalObject>" ; extend-method*
ObjList ' pr-str :noname drop s" #<ObjList>" ; extend-method*
ObjList ' conj :noname ." not yet done" ; extend-method*

\ Run some protocol methods!
ObjList new pr-str type
ObjList new conj

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


)


cr
bye
." Done loading" cr
