require reader.fs
require printer.fs

: args-as-native { argv argc -- entry*argc... }
    argc 0 ?do
        argv i cells + @ as-native
    loop ;

: env-assoc ( map sym-str-addr sym-str-len xt )
    -rot MalSymbol. swap MalNativeFn. rot assoc ;

MalMap/Empty
    s" +" :noname args-as-native + MalInt. ; env-assoc
    s" -" :noname args-as-native - MalInt. ; env-assoc
    s" *" :noname args-as-native * MalInt. ; env-assoc
    s" /" :noname args-as-native / MalInt. ; env-assoc
constant repl-env

: read read-str ;
: eval ( env obj ) mal-eval ;
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
            env  list MalList/start @ 2 cells + @  eval
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

MalSymbol
  extend mal-eval { env sym -- val }
    0 sym env get
    dup 0= if
        drop
        0 0 s" ' not found" sym pr-str s" '" ...throw-str
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
    list MalList/count @ 0= if
        list
    else
        env list MalList/start @ @ eval
        env list rot eval-invoke
    endif ;;
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
      dup 0 <> if
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
      endif
    repeat ;

read-lines
cr
bye
