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

: is-pair? ( obj -- bool )
    empty? mal-false = ;

defspecial quote ( env list -- form )
    nip MalList/start @ cell+ @ ;;

s" concat" MalSymbol. constant concat-sym
s" cons" MalSymbol. constant cons-sym

defer quasiquote
: quasiquote0 { ast -- form }
    ast is-pair? 0= if
        here quote-sym , ast , here>MalList
    else
        ast to-list MalList/start @ { ast-start }
        ast-start @ { ast[0] }
        ast[0] unquote-sym m= if
            ast-start cell+ @
        else
            ast[0] is-pair? if
                ast[0] to-list MalList/start @ { ast[0]-start }
                ast[0]-start @ splice-unquote-sym m= if
                    here
                    concat-sym ,
                    ast[0]-start cell+ @ ,
                    ast to-list MalList/rest quasiquote ,
                    here>MalList
                    false
                else true endif
            else true endif
            if
                here
                cons-sym ,
                ast[0] quasiquote ,
                ast to-list MalList/rest quasiquote ,
                here>MalList
            endif
        endif
    endif ;
' quasiquote0 is quasiquote

defspecial quasiquote ( env list )
    MalList/start @ cell+ @ ( ast )
    quasiquote TCO-eval ;;

defspecial def! { env list -- val }
    list MalList/start @ cell+ { arg0 }
    arg0 @ ( key )
    env arg0 cell+ @ eval dup { val } ( key val )
    env env/set val ;;

defspecial defmacro! { env list -- val }
    list MalList/start @ cell+ { arg0 }
    arg0 @ ( key )
    env arg0 cell+ @ eval { val }
    true val MalUserFn/is-macro? !
    val env env/set
    val ;;

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

: new-user-fn-env { argv argc mal-fn -- env }
    mal-fn MalUserFn/formal-args @ { f-args-list }
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
    env ;

MalUserFn
  extend eval-invoke { call-env list mal-fn -- list }
    mal-fn MalUserFn/is-macro? @ if
        list MalList/start @ cell+  list MalList/count @ 1-
    else
        call-env list eval-rest
    endif
    mal-fn new-user-fn-env { env }

    mal-fn MalUserFn/is-macro? @ if
        env   mal-fn MalUserFn/body @   eval
        env swap TCO-eval
    else
        env   mal-fn MalUserFn/body @   TCO-eval
    endif ;;
drop

defspecial fn* { env list -- val }
    list MalList/start @ cell+ { arg0 }
    MalUserFn new
    env over MalUserFn/env !
    arg0 @ to-list over MalUserFn/formal-args !
    arg0 cell+ @ over MalUserFn/body ! ;;

defspecial macroexpand ( env list[_,form] -- form )
    MalList/start @ cell+ @ swap over ( form env form )
    MalList/start @ @ ( form env macro-name-expr )
    eval { macro-fn } ( form )
    dup MalList/start @ cell+  swap MalList/count @ 1- macro-fn ( argv argc fn )
    new-user-fn-env ( env )
    macro-fn MalUserFn/body @   TCO-eval ;;

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

defcore eval ( argv argc )
  drop @ repl-env swap eval ;;

: rep ( str-addr str-len -- str-addr str-len )
    read
    repl-env swap eval
    print ;

: mk-args-list ( -- )
    here
    begin
        next-arg 2dup 0 0 d<> while
            MalString. ,
    repeat
    2drop here>MalList ;

create buff 128 allot
77777777777 constant stack-leak-detect

s\" (def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" rep 2drop
s\" (defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" rep 2drop
s\" (defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))" rep 2drop

: repl ( -- )
    begin
      ." user> "
      stack-leak-detect
      buff 128 stdin read-line throw
    while ( num-bytes-read )
      buff swap ( str-addr str-len )
      ['] rep
      \ execute type
      catch ?dup 0= if safe-type else ." Caught error " . endif
      cr
      stack-leak-detect <> if ." --stack leak--" cr endif
    repeat ;

: main ( -- )
    mk-args-list { args-list }
    args-list MalList/count @ 0= if
        s" *ARGV*" MalSymbol. MalList/Empty repl-env env/set
        repl
    else
        args-list MalList/start @ @ { filename }
        s" *ARGV*" MalSymbol. args-list MalList/rest repl-env env/set

        repl-env
        here s" load-file" MalSymbol. , filename , here>MalList
        eval print
    endif ;

main
cr
bye
