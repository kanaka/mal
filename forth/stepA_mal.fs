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
  extend invoke { argv argc kw -- val }
    0   kw   argv @   get
    ?dup 0= if
        argc 1 > if
            argv cell+ @
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
  extend eval-invoke { env list this -- list }
    env list eval-rest ( argv argc )
    this invoke ;;
  extend invoke ( argv argc this -- val )
    MalNativeFn/xt @ execute ;;
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
        \ pass empty list for last arg, unless overridden below
        1- cells f-args + @ MalList new env env/set
    endif
    argc 0 ?do
        f-args i cells + @
        dup &-sym m= if
            drop
            argc i - { c }
            c cells allocate throw { start }
            argv i cells +  start  c cells  cmove
            f-args i 1+ cells + @ ( more-args-symbol )
            start c MalList. env env/set
            leave
        endif
        argv i cells + @
        env env/set
    loop
    env ;

MalUserFn
  extend eval-invoke { call-env list mal-fn -- list }
    mal-fn MalUserFn/is-macro? @ if
        list MalList/start @ cell+  \ argv
        list MalList/count @ 1-     \ argc
        mal-fn new-user-fn-env { env }
        env   mal-fn MalUserFn/body @   eval
        call-env swap TCO-eval
    else
        call-env list eval-rest
        mal-fn invoke
    endif ;;

  extend invoke ( argv argc mal-fn )
    dup { mal-fn } new-user-fn-env { env }
    env   mal-fn MalUserFn/body @   TCO-eval ;;
drop

defspecial fn* { env list -- val }
    list MalList/start @ cell+ { arg0 }
    MalUserFn new
    false over MalUserFn/is-macro? !
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

5555555555 constant pre-try

defspecial try* { env list -- val }
    list MalList/start @ cell+ { arg0 }
    pre-try
    env arg0 @ ['] eval catch ?dup 0= if
        nip
    else { errno }
        begin pre-try = until
        errno 1 <> if
            s" forth-errno" MalKeyword. errno MalInt. MalMap/Empty assoc
            to exception-object
        endif
        arg0 cell+ @ ( list[catch*,sym,form] )
        MalList/start @ cell+ { catch0 }
        env MalEnv. { catch-env }
        catch0 @ exception-object catch-env env/set
        catch-env  catch0 cell+ @  TCO-eval
    endif ;;

defspecial . { env coll -- rtn-list }
    depth { old-depth }
    coll to-list dup MalList/count @ swap MalList/start @ { count start }
    count cells start +  start cell+  +do
        env i @ eval as-native
    cell +loop ;;

MalSymbol
  extend mal-eval { env sym -- val }
    sym env env/get-addr
    dup 0= if
        drop
        0 0 s" ' not found" sym pr-str s" '" ...throw-str
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

: nop ;

defcore swap! { argv argc -- val }
    \ argv is  (atom fn args...)
    argv @ { atom }
    argv cell+ @ { fn }
    argc 1- { call-argc }
    call-argc cells allocate throw { call-argv }
    atom Atom/val   call-argv    1 cells   cmove
    argv cell+ cell+   call-argv cell+   call-argc 1- cells   cmove
    call-argv call-argc fn  invoke
    dup TCO-eval = if drop eval endif { new-val }
    new-val atom Atom/val !
    new-val ;;

defcore map ( argv argc -- list )
    drop dup @ swap cell+ @ to-list { fn list }
    here
    list MalList/start @   list MalList/count @  cells   over + swap   +do
        i 1 fn invoke
        dup TCO-eval = if drop eval endif
        ,
    cell +loop
    here>MalList ;;

defcore readline ( argv argc -- mal-string )
    drop @ unpack-str type stdout flush-file drop
    buff 128 stdin read-line throw
    if buff swap MalString. else drop mal-nil endif ;;

s\" (def! *host-language* \"forth\")" rep 2drop
s\" (def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" rep 2drop
s\" (defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" rep 2drop
s\" (def! *gensym-counter* (atom 0))" rep 2drop
s\" (def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))" rep 2drop
s\" (defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))" rep 2drop

: repl ( -- )
    s\" (println (str \"Mal [\" *host-language* \"]\"))" rep 2drop
    begin
      ." user> "
      stack-leak-detect
      buff 128 stdin read-line throw
    while ( num-bytes-read )
      buff swap ( str-addr str-len )
      ['] rep
      \ execute ['] nop \ uncomment to see stack traces
      catch ?dup 0= if
          safe-type cr
          stack-leak-detect <> if ." --stack leak--" cr endif
      else { errno }
          begin stack-leak-detect = until
          errno 1 <> if
              s" forth-errno" MalKeyword. errno MalInt. MalMap/Empty assoc
              to exception-object
          endif
          ." Uncaught exception: "
          exception-object pr-str safe-type cr
      endif
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
