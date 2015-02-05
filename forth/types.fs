\ === tiny framework for inline tests === /
: test=
  2dup = if
    2drop
  else
    cr ." assert failed on line " sourceline# .
    swap cr ." | got " . cr ." | expected " . cr
  endif ;

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

\ define a function to append a space
bl c,
here constant space-str
: a-space space-str 1 str-append ;

: int>str ( num -- str-addr str-len )
  s>d <# #s #> ;


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

\ nil type and instance to support extending protocols to it
MalType% deftype* constant MalNil
MalNil new constant mal-nil

\ Example and tests

MalType%
  cell% field MalList/car
  cell% field MalList/cdr
deftype* constant MalList

: MalList/conj { val coll -- list }
    MalList new ( list )
    val over MalList/car ! ( list )
    coll over MalList/cdr ! ( list )
    ;

MalList new
MalList new
= 0 test=

MalList new dup MalList/car 5 swap ! MalList/car @   5 test=


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
    2drop drop MalDefault dup MalTypeType-methods 2@ swap
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
      \ cr ." before: " MalList MalTypeType-method-vals @ @ . cr
      type MalTypeType-method-vals @ idx ixt array-insert ( new-array )
        type MalTypeType-method-vals !
      \ cr ." after: " MalList MalTypeType-method-vals @ @ . cr
    endif
  endif
  type
  ;


\ def-protocol-method pr-str   ...can be written:
\ : pr-str ( obj -- str ) [ latestxt ] literal execute-method ;
: def-protocol-method ( "name" -- )
    create latestxt ,
  does> ( ??? obj xt-ref -- ??? )
    @ execute-method ;

: extend ( type -- type pxt <noname...>)
    parse-name find-name name>int ( type pxt )
    :noname
    ;

: ;; ( type pxt <noname...> -- type )
    [compile] ; ( type pxt ixt )
    extend-method*
    ; immediate

(
\ These whole-protocol names are only needed for 'satisfies?':
protocol IPrintable
  def-protocol-method pr-str
end-protocol

MalList IPrintable extend
  ' pr-str :noname drop s" <unprintable>" ; extend-method*

  extend-method pr-str
    drop s" <unprintable>" ;;
end-extend
)

\ Examples of making new protocol methods (without a protocol to group them yet!)
def-protocol-method pr-buf ( str-addr str-len this -- str-addr str-len )
def-protocol-method conj ( obj this -- this )

: pr-str { obj }
    new-str obj pr-buf ;

\ Examples of extending existing protocol methods to existing type
MalDefault
  extend pr-buf
    { this }
    s" #<MalObject" str-append a-space
    this int>str str-append
    s" >" str-append ;;
  extend conj   ( obj this -- this )
    swap drop ;;
drop

MalNil
  extend pr-buf
    drop s" nil" str-append ;;
  ' conj ' MalList/conj extend-method*
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
  ' conj ' MalList/conj extend-method*
drop


MalType%
  cell% field MalInt/int
deftype* constant MalInt

MalInt
  extend pr-buf
    MalInt/int @ int>str str-append ;;
drop

: MalInt. { int -- mal-int }
    MalInt new dup MalInt/int int swap ! ;


\ Run some protocol methods!

mal-nil
42 MalInt. mal-nil conj
10 MalInt. mal-nil conj conj
20 MalInt. swap conj
23 MalInt. mal-nil conj conj conj
pr-str s" (nil (20 (42) 10) 23)" str= -1 test=
