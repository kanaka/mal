require reader.fs
require printer.fs

: args-as-native { argv argc -- entry*argc... }
    argc 0 ?do
        argv i cells + @ as-native
    loop ;

MalMap/Empty
    s" +" MalSymbol. :noname args-as-native + MalInt. ; MalFn. rot assoc
    s" -" MalSymbol. :noname args-as-native - MalInt. ; MalFn. rot assoc
    s" *" MalSymbol. :noname args-as-native * MalInt. ; MalFn. rot assoc
    s" /" MalSymbol. :noname args-as-native / MalInt. ; MalFn. rot assoc
value repl-env

def-protocol-method mal-eval ( env ast -- val )
def-protocol-method mal-eval-ast ( env ast -- val )

MalDefault extend mal-eval nip ;; drop

MalSymbol
  extend mal-eval { env sym -- val }
    0 sym env get
    dup 0= if
        drop
        ." Symbol '"
        sym as-native safe-type
        ." ' not found." cr
        1 throw
    endif ;;
drop

MalArray
  extend mal-eval { env ary -- val }
    \ Pass args on dictionary stack (!)
    \ TODO: consider allocate and free of a real MalArray instead
    here { val-start }
    ary MalArray/start @ { expr-start }
    ary MalArray/count @ 0 ?do
        env expr-start i cells + @ mal-eval ,
    loop
    val-start cell+  here val-start - cell / 1-  val-start @  ( argv argc MalFn )
    invoke
    val-start here - allot ;;
  extend mal-eval-ast { env ary -- ary }
    here
    ary MalArray/start @ { expr-start }
    ary MalArray/count @ 0 ?do
        env expr-start i cells + @ mal-eval ,
    loop
    here>MalArray ;;
drop

MalList
  extend mal-eval-ast { env list -- ary }
    here
    list
    begin ( list )
        dup mal-nil <>
    while
        env over MalList/car @ mal-eval ,
        MalList/cdr @
    repeat
    drop here>MalArray ;;
drop

MalVector
  extend mal-eval ( env vector -- vector )
    MalVector/list @ mal-eval-ast
    MalVector new swap over MalVector/list ! ;;
drop

MalMap
  extend mal-eval ( env map -- map )
    MalMap/list @ mal-eval-ast
    MalMap new swap over MalMap/list ! ;;
drop

: read read-str ;
: eval ( env obj ) mal-eval ;
: print
    \ ." Type: " dup mal-type @ type-name safe-type cr
    pr-str ;

: rep ( str -- val )
    read
    repl-env swap eval
    print ;

create buff 128 allot

: read-lines
    begin
      ." user> "
      buff 128 stdin read-line throw
    while
      buff swap
      ['] rep
      \ execute safe-type
      catch 0= if safe-type else ." Caught error" endif
      cr
    repeat ;

read-lines
cr
bye
