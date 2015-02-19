require str.fs
require types.fs

\ === printer protocol and implementations === /

def-protocol-method pr-buf ( readably? str-addr str-len this -- str-addr str-len )
def-protocol-method pr-seq-buf ( readably? str-addr str-len this -- str-addr str-len )

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
    rot { list }
    list MalList/count @ { count }
    count 0 > if
        list MalList/start @ { start }
        start @ pr-buf a-space start cell+ @ pr-buf
        count 2 / 1 ?do
            s" , " str-append
            start i 2 * cells + @ pr-buf a-space
            start i 2 * 1+ cells + @ pr-buf
        loop
    endif
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

Atom
  extend pr-buf { this }
    s" (atom " str-append
    this Atom/val @ pr-buf
    s" )" str-append ;;
drop