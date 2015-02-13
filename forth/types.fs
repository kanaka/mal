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
        nip            ( start middle )
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
  cell% field MalTypeType-name-addr
  cell% field MalTypeType-name-len
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
  dup MalTypeType-name-len    0   swap !   ( MalTypeType )
  ;

: deftype ( struct-align struct-len R:type-name -- )
    parse-name { orig-name-addr name-len }
    \ parse-name uses temporary space, so copy into dictionary stack:
    here { name-addr } name-len allot
    orig-name-addr name-addr name-len cmove

    \ allot and initialize type structure
    deftype* { mt }
    name-addr mt MalTypeType-name-addr !
    name-len  mt MalTypeType-name-len  !
    \ ." Defining " mt MalTypeType-name-addr @ mt MalTypeType-name-len @ type cr
    mt name-addr name-len nextname 1 0 const-does> ;

: type-name ( mal-type )
    dup  MalTypeType-name-addr @ ( mal-type name-addr )
    swap MalTypeType-name-len @ ( name-addr name-len )
    ;

MalType% deftype MalDefault

\ nil type and instance to support extending protocols to it
MalType% deftype MalNil
MalNil new constant mal-nil

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
  0= if ( type idx )
      2drop
      ." No protocol fn '"
      pxt >name name>string type
      ." ' extended to type '"
      obj mal-type @ type-name type
      ." '"
      1 throw
  endif

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

\ === Mal types and protocols === /

MalType%
  cell% field MalList/car
  cell% field MalList/cdr
deftype MalList

: MalList/conj { val coll -- list }
    MalList new ( list )
    val over MalList/car ! ( list )
    coll over MalList/cdr ! ( list )
    ;

MalType%
  cell% field MalArray/count
  cell% field MalArray/start
deftype MalArray

: here>MalArray ( old-here -- mal-array )
    here over - { bytes } ( old-here )
    MalArray new bytes ( old-here mal-array bytes )
    allocate throw dup { target } over MalArray/start ! ( old-here mal-array )
    bytes cell / over MalArray/count ! ( old-here mal-array )
    swap target bytes cmove ( mal-array )
    0 bytes - allot \ pop array contents from dictionary stack
    ;

def-protocol-method conj ( obj this -- this )
def-protocol-method assoc ( k v this -- this )
def-protocol-method get ( not-found k this -- value )
def-protocol-method mal= ( a b -- bool )
def-protocol-method as-native ( obj --  )
def-protocol-method invoke ( argv argc mal-fn -- ... )

MalType%
  cell% field MalVector/list
deftype MalVector

MalType%
  cell% field MalMap/list
deftype MalMap

MalMap new mal-nil over MalMap/list ! constant MalMap/Empty

MalMap
  extend conj ( kv map -- map )
    MalMap/list @ \ get list
    over MalList/cdr @ MalList/car @ conj \ add value
    swap MalList/car @ conj \ add key
    MalMap new MalMap/list ! \ put back in map
    ;;
  extend assoc ( k v map -- map )
    MalMap/list @ \ get list
    conj conj
    MalMap new dup -rot MalMap/list ! \ put back in map
    ;;
  extend get ( not-found k map -- value )
    -rot { not-found k }
    MalMap/list @ \ get list
    begin
        dup MalList/cdr @
        swap MalList/car @ k mal= if
            MalList/car @ -1 \ found it
        else
            MalList/cdr @
            dup mal-nil = if
                not-found -1
            else
                0
            endif
        endif
    until ;;
drop

\ Examples of extending existing protocol methods to existing type
MalDefault
  extend conj   ( obj this -- this )
    nip ;;
  extend as-native ;; ( obj -- obj )
drop

MalNil
  ' conj ' MalList/conj extend-method*
  extend as-native drop 0 ;;
drop

MalList
  ' conj ' MalList/conj extend-method*
drop


MalType%
  cell% field MalInt/int
deftype MalInt

: MalInt. { int -- mal-int }
    MalInt new dup MalInt/int int swap ! ;

MalInt
  extend as-native ( mal-int -- int )
    MalInt/int @ ;;
drop

MalType%
  cell% field MalSymbol/sym-addr
  cell% field MalSymbol/sym-len
  cell% field MalSymbol/meta
deftype MalSymbol

: MalSymbol. { str-addr str-len -- mal-sym }
    MalSymbol new { sym }
    str-addr sym MalSymbol/sym-addr !
    str-len  sym MalSymbol/sym-len  !
    MalMap/Empty sym MalSymbol/meta  !
    sym ;

: unpack-sym ( mal-string -- addr len )
    dup MalSymbol/sym-addr @
    swap MalSymbol/sym-len @ ;

MalSymbol
  extend mal= ( other this -- bool )
    over mal-type @ MalSymbol = if
        unpack-sym rot unpack-sym str=
    else
        2drop 0
    endif ;;
  ' as-native ' unpack-sym extend-method*
drop

MalType%
  cell% field MalKeyword/str-addr
  cell% field MalKeyword/str-len
deftype MalKeyword

: unpack-keyword ( mal-keyword -- addr len )
    dup MalKeyword/str-addr @
    swap MalKeyword/str-len @ ;

MalKeyword
  extend mal= ( other this -- bool )
    over mal-type @ MalKeyword = if
        unpack-keyword rot unpack-keyword str=
    else
        2drop 0
    endif ;;
  ' as-native ' unpack-keyword extend-method*
  extend invoke { argv argc kw -- val }
    argc 1 > if argv cell+ @ else mal-nil endif \ not-found
    kw \ key
    argv @ \ map
    get ;;
drop

: MalKeyword. { str-addr str-len -- mal-keyword }
    MalKeyword new { kw }
    str-addr kw MalKeyword/str-addr !
    str-len  kw MalKeyword/str-len  !
    kw ;

MalType%
  cell% field MalString/str-addr
  cell% field MalString/str-len
deftype MalString

: MalString. { str-addr str-len -- mal-str }
    MalString new { str }
    str-addr str MalString/str-addr !
    str-len  str MalString/str-len  !
    str ;

: unpack-str ( mal-string -- addr len )
    dup MalString/str-addr @
    swap MalString/str-len @ ;

MalString
  extend mal= ( other this -- bool )
    over mal-type @ MalString = if
        unpack-str rot unpack-str str=
    else
        2drop 0
    endif ;;
  ' as-native ' unpack-str extend-method*
drop


MalType%
  cell% field MalFn/xt
  cell% field MalFn/meta
deftype MalFn

: MalFn. { xt -- mal-fn }
    MalFn new { mal-fn }
    xt mal-fn MalFn/xt !
    MalMap/Empty mal-fn MalFn/meta  !
    mal-fn ;

MalFn
  extend invoke ( ... mal-fn -- ... )
    MalFn/xt @ execute ;;
  extend as-native
    MalFn/xt @ ;;
drop
