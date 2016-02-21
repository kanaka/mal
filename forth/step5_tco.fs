require reader.fs
require printer.fs
require core.fs

core MalEnv. constant repl-env

99999999 constant TCO-eval

: read read-str ;
: eval ( env obj )
    begin
        \ ." eval-> " dup pr-str safe-type cr
        mal-eval
        dup TCO-eval =
    while
        drop
    repeat ;
: print
    \ ." Type: " dup mal-type @ type-name safe-type cr
    pr-str ;

MalDefault extend mal-eval nip ;; drop \ By default, evalutate to yourself

MalKeyword
  extend eval-invoke { env list kw -- val }
    0   kw   env list MalList/start @ cell+ @ eval   get
    ?dup 0= if
        \ compute not-found value
        list MalList/count @ 1 > if
            env  list MalList/start @ 2 cells + @  TCO-eval
        else
            mal-nil
        endif
    endif ;;
drop

\ eval all but the first item of list
: eval-rest { env list -- argv argc }
    list MalList/start @ cell+ { expr-start }
    list MalList/count @ 1- { argc }
    argc cells allocate throw { target }
    argc 0 ?do
        env expr-start i cells + @ eval
        target i cells + !
    loop
    target argc ;

MalNativeFn
  extend eval-invoke ( env list this -- list )
    MalNativeFn/xt @ { xt }
    eval-rest ( argv argc )
    xt execute ( return-val ) ;;
drop

SpecialOp
  extend eval-invoke ( env list this -- list )
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
    env arg0 cell+ @ eval dup { val } ( key val )
    env env/set val ;;

defspecial let* { old-env list -- val }
    old-env MalEnv. { env }
    list MalList/start @ cell+ dup { arg0 }
    @ to-list
    dup MalList/start @ { bindings-start } ( list )
    MalList/count @ 0 +do
        bindings-start i cells + dup @ swap cell+ @ ( sym expr )
        env swap eval
        env env/set
    2 +loop
    env arg0 cell+ @ TCO-eval
    \ TODO: dec refcount of env
    ;;

defspecial do { env list -- val }
    list MalList/start @ { start }
    list MalList/count @ dup 1- { last } 1 ?do
        env   start i cells + @
        i last = if
            TCO-eval
        else
            eval drop
        endif
    loop ;;

defspecial if { env list -- val }
    list MalList/start @ cell+ { arg0 }
    env arg0 @ eval ( test-val )
    dup mal-false = if
        drop -1
    else
        mal-nil =
    endif
    if
        \ branch to false
        list MalList/count @ 3 > if
            env arg0 cell+ cell+ @ TCO-eval
        else
            mal-nil
        endif
    else
        \ branch to true
        env arg0 cell+ @ TCO-eval
    endif ;;

s" &" MalSymbol. constant &-sym

MalUserFn
  extend eval-invoke { call-env list mal-fn -- list }
    call-env list eval-rest { argv argc }

    mal-fn MalUserFn/formal-args @ { f-args-list }
    mal-fn MalUserFn/env @ MalEnv. { env }

    f-args-list MalList/start @ { f-args }
    f-args-list MalList/count @ ?dup 0= if else
        \ pass empty list for last arg, unless overridden below
        1- cells f-args + @ MalList new env env/set
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

    env   mal-fn MalUserFn/body @   TCO-eval ;;
drop

defspecial fn* { env list -- val }
    list MalList/start @ cell+ { arg0 }
    MalUserFn new
    env over MalUserFn/env !
    arg0 @ to-list over MalUserFn/formal-args !
    arg0 cell+ @ over MalUserFn/body ! ;;

MalSymbol
  extend mal-eval { env sym -- val }
    sym env env/get-addr
    dup 0= if
        drop
        ." Symbol '" sym pr-str safe-type ." ' not found." cr
        1 throw
    else
        @
    endif ;;
drop

: eval-ast { env list -- list }
    here
    list MalList/start @ { expr-start }
    list MalList/count @ 0 ?do
        env expr-start i cells + @ eval ,
    loop
    here>MalList ;

MalList
  extend mal-eval { env list -- val }
    env list MalList/start @ @ eval
    env list rot eval-invoke ;;
drop

MalVector
  extend mal-eval ( env vector -- vector )
    MalVector/list @ eval-ast
    MalVector new swap over MalVector/list ! ;;
drop

MalMap
  extend mal-eval ( env map -- map )
    MalMap/list @ eval-ast
    MalMap new swap over MalMap/list ! ;;
drop

: rep ( str-addr str-len -- str-addr str-len )
    read
    repl-env swap eval
    print ;

create buff 128 allot
77777777777 constant stack-leak-detect

: read-lines
    begin
      ." user> "
      stack-leak-detect
      buff 128 stdin read-line throw
    while ( num-bytes-read )
      buff swap ( str-addr str-len )
      ['] rep
      \ execute safe-type
      catch ?dup 0= if safe-type else ." Caught error " . endif
      cr
      stack-leak-detect <> if ." --stack leak--" cr endif
    repeat ;

read-lines
cr
bye
