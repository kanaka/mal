require reader.fs
require printer.fs
require env.fs

: args-as-native { argv argc -- entry*argc... }
    argc 0 ?do
        argv i cells + @ as-native
    loop ;

0 MalEnv. constant repl-env
s" +" MalSymbol.  :noname args-as-native + MalInt. ; MalFn.  repl-env env/set
s" -" MalSymbol.  :noname args-as-native - MalInt. ; MalFn.  repl-env env/set
s" *" MalSymbol.  :noname args-as-native * MalInt. ; MalFn.  repl-env env/set
s" /" MalSymbol.  :noname args-as-native / MalInt. ; MalFn.  repl-env env/set

def-protocol-method mal-eval ( env ast -- val )
def-protocol-method mal-eval-ast ( env ast -- val )
def-protocol-method invoke+ ( env arty -- ... )
def-protocol-method invoke ( argv argc mal-fn -- ... )

MalDefault extend mal-eval nip ;; drop

MalKeyword
  extend invoke { argv argc kw -- val }
    argc 1 > if argv cell+ @ else mal-nil endif \ not-found
    kw \ key
    argv @ \ map
    get ;;
drop

MalFn
  extend invoke ( ... mal-fn -- ... )
    MalFn/xt @ execute ;;

  extend invoke+ { env ary this -- ary }
    \ Pass args on dictionary stack (!)
    \ TODO: consider allocate and free of a real MalArray instead
    \ Normal list, evaluate and invoke
    here { val-start }
    ary MalArray/start @ { expr-start }
    ary MalArray/count @ 1 ?do
        env expr-start i cells + @ mal-eval ,
    loop
    val-start  here val-start - cell /  this  ( argv argc MalFn )
    invoke
    val-start here - allot ;;
drop

SpecialOp
  extend invoke+ ( env ary this -- ary )
    SpecialOp/xt @ execute ;;
drop

s" quote" MalSymbol. :noname ( env ary -- form )
    nip MalArray/start @ cell+ @
; SpecialOp. repl-env env/set

s" def!" MalSymbol. :noname { env ary -- }
    ary MalArray/start @ cell+ { arg0 }
    arg0 @ ( key )
    env arg0 cell+ @ mal-eval dup { val } ( key val )
    env env/set
    val
; SpecialOp. repl-env env/set

s" let*" MalSymbol. :noname { old-env ary -- }
    old-env MalEnv. { env }
    ary MalArray/start @ cell+ dup { arg0 }
    @ to-array
    dup MalArray/start @ { bindings-start } ( ary )
    MalArray/count @ 0 +do
        bindings-start i cells + dup @ swap cell+ @ ( sym expr )
        env swap mal-eval
        env env/set
    2 +loop
    env arg0 cell+ @ mal-eval
    \ TODO: dec refcount of env
; SpecialOp. repl-env env/set

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
    env ary MalArray/start @ @ mal-eval
    env ary rot invoke+ ;;

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
      42042042042
      buff 128 stdin read-line throw
    while
      buff swap
      ['] rep
      execute safe-type
      \ catch 0= if safe-type else ." Caught error" endif
      cr
      42042042042 <> if ." --stack leak--" cr endif
    repeat ;

read-lines
cr
bye
