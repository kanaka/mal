require reader.fs
require printer.fs

: args-as-native { argv argc -- entry*argc... }
    argc 0 ?do
        argv i cells + @ as-native
    loop ;

MalMap/Empty
    s" +" MalSymbol. :noname args-as-native + MalInt. ; MalNativeFn. rot assoc
    s" -" MalSymbol. :noname args-as-native - MalInt. ; MalNativeFn. rot assoc
    s" *" MalSymbol. :noname args-as-native * MalInt. ; MalNativeFn. rot assoc
    s" /" MalSymbol. :noname args-as-native / MalInt. ; MalNativeFn. rot assoc
value repl-env

def-protocol-method mal-eval ( env ast -- val )
def-protocol-method mal-eval-ast ( env ast -- val )
def-protocol-method invoke ( argv argc mal-fn -- ... )

MalDefault extend mal-eval nip ;; drop

MalKeyword
  extend invoke { argv argc kw -- val }
    argc 1 > if argv cell+ @ else mal-nil endif \ not-found
    kw \ key
    argv @ \ map
    get ;;
drop

MalNativeFn
  extend invoke ( ... mal-fn -- ... )
    MalNativeFn/xt @ execute ;;
drop

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

MalList
  extend mal-eval { env list -- val }
    \ Pass args on dictionary stack (!)
    \ TODO: consider allocate and free of a real MalList instead
    here { val-start }
    list MalList/start @ { expr-start }
    list MalList/count @ 0 ?do
        env expr-start i cells + @ mal-eval ,
    loop
    val-start cell+  here val-start - cell / 1-  val-start @  ( argv argc MalNativeFn )
    invoke
    val-start here - allot ;;
  extend mal-eval-ast { env list -- list }
    here
    list MalList/start @ { expr-start }
    list MalList/count @ 0 ?do
        env expr-start i cells + @ mal-eval ,
    loop
    here>MalList ;;
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
