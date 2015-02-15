require reader.fs
require printer.fs
require core.fs

core MalEnv. constant repl-env

\ Fully evalutate any Mal object:
def-protocol-method mal-eval ( env ast -- val )

\ Invoke an object, given whole env and unevaluated argument forms:
def-protocol-method invoke ( argv argc mal-fn -- ... )

MalDefault extend mal-eval nip ;; drop

MalKeyword
  extend invoke { env list kw -- val }
    0   kw   env list MalList/start @ cell+ @ mal-eval   get
    ?dup 0= if
        \ compute not-found value
        list MalList/count @ 1 > if
            env  list MalList/start @ 2 cells + @  mal-eval
        else
            mal-nil
        endif
    endif ;;
drop

\ eval all but the first item of list, storing in temporary memory
\ that should be freed with free-eval-rest when done.
: eval-rest { env list -- mem-token argv argc }
    \ Pass args on dictionary stack (!)
    \ TODO: consider allocate and free of a real MalList instead
    \ Normal list, evaluate and invoke
    here { val-start }
    list MalList/start @ cell+ { expr-start }
    list MalList/count @ 1- dup { argc } 0 ?do
        env expr-start i cells + @ mal-eval ,
    loop
    val-start  val-start  argc ;

: free-eval-rest ( mem-token/val-start -- )
    here - allot ;

MalNativeFn
  extend invoke ( env list this -- list )
    MalNativeFn/xt @ { xt }
    eval-rest ( mem-token argv argc )
    xt execute ( mem-token return-val )
    swap free-eval-rest ;;
drop

SpecialOp
  extend invoke ( env list this -- list )
    SpecialOp/xt @ execute ;;
drop

: install-special ( symbol xt )
    SpecialOp. repl-env env/set ;

: defspecial
    parse-allot-name MalSymbol.
    ['] install-special
    :noname
    ;

defspecial quote ( env list -- form )
    nip MalList/start @ cell+ @ ;;

defspecial def! { env list -- val }
    list MalList/start @ cell+ { arg0 }
    arg0 @ ( key )
    env arg0 cell+ @ mal-eval dup { val } ( key val )
    env env/set
    val ;;

defspecial let* { old-env list -- val }
    old-env MalEnv. { env }
    list MalList/start @ cell+ dup { arg0 }
    @ to-list
    dup MalList/start @ { bindings-start } ( list )
    MalList/count @ 0 +do
        bindings-start i cells + dup @ swap cell+ @ ( sym expr )
        env swap mal-eval
        env env/set
    2 +loop
    env arg0 cell+ @ mal-eval
    \ TODO: dec refcount of env
    ;;

defspecial do { env list -- val }
    list MalList/start @
    0
    list MalList/count @ 1 ?do
        drop
        dup i cells + @ env swap mal-eval
    loop
    nip ;;

defspecial if { env list -- val }
    list MalList/start @ cell+ { arg0 }
    env arg0 @ mal-eval ( test-val )
    dup mal-false = if
        drop -1
    else
        mal-nil =
    endif
    if
        \ branch to false
        list MalList/count @ 3 > if
            env arg0 cell+ cell+ @ mal-eval
        else
            mal-nil
        endif
    else
        \ branch to true
        env arg0 cell+ @ mal-eval
    endif ;;

s" &" MalSymbol. constant &-sym

MalUserFn
  extend invoke { call-env list mal-fn -- list }
    call-env list eval-rest { mem-token argv argc }

    mal-fn MalUserFn/formal-args @ { f-args-list }
    \ \ This isn't correct for fns with & in their f-args-list:
    \ f-args-list MalList/count @ argc 2dup = if
    \     2drop
    \ else
    \     ." Argument mismatch on user fn. Got " . ." but expected " . cr
    \     1 throw
    \ endif

    mal-fn MalUserFn/env @ MalEnv. { env }

    f-args-list MalList/start @ { f-args }
    f-args-list MalList/count @ ?dup 0= if else
        \ pass nil for last arg, unless overridden below
        1- cells f-args + @ mal-nil env env/set
    endif
    argc 0 ?do
        f-args i cells + @
        dup &-sym m= if
            drop
            f-args i 1+ cells + @ ( more-args-symbol )
            MalList new ( sym more-args )
            argc i - dup { c } over MalList/count !
            c cells allocate throw dup { start } over MalList/start !
            argv i cells +  start  c cells  cmove
            env env/set
            leave
        endif
        argv i cells + @
        env env/set
    loop

    env   mal-fn MalUserFn/body @   mal-eval

    mem-token free-eval-rest ;;

defspecial fn* { env list -- val }
    list MalList/start @ cell+ { arg0 }
    MalUserFn new
    env over MalUserFn/env !
    arg0 @ to-list over MalUserFn/formal-args !
    arg0 cell+ @ over MalUserFn/body ! ;;

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

: mal-eval-ast { env list -- list }
    here
    list MalList/start @ { expr-start }
    list MalList/count @ 0 ?do
        env expr-start i cells + @ mal-eval ,
    loop
    here>MalList ;

MalList
  extend mal-eval { env list -- val }
    env list MalList/start @ @ mal-eval
    env list rot invoke ;;
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
      77777777777
      buff 128 stdin read-line throw
    while
      buff swap
      ['] rep
      execute safe-type
      \ catch 0= if safe-type else ." Caught error" endif
      cr
      77777777777 <> if ." --stack leak--" cr endif
    repeat ;

read-lines
cr
bye
