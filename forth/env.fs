require types.fs

MalType%
   cell% field MalEnv/outer
   cell% field MalEnv/data
deftype MalEnv

: MalEnv. { outer -- env }
    MalEnv new { env }
    outer env MalEnv/outer !
    MalMap/Empty env MalEnv/data !
    env ;

: env/set { key val env -- }
    key val env MalEnv/data @ assoc
    env MalEnv/data ! ;

: env/find { key env -- env-or-0 }
    env
    begin ( env )
        dup 0 key rot MalEnv/data @ get ( env val-or-0 )
        0= if ( env )
            MalEnv/outer @ dup 0= ( env-or-0 done-looping? )
        else
            -1 \ found it! ( env -1 )
        endif
    until ;

MalEnv
  extend get { not-found key env -- }
    key env env/find ( env-or-0 )
    ?dup 0= if
        not-found
    else ( env )
        not-found key rot MalEnv/data @ get
    endif ;;
  extend pr-buf { env }
    env MalEnv/data @ pr-buf
    a-space s" outer: " str-append
    env MalEnv/outer @ ?dup 0= if
        s" <none>" str-append
    else
        pr-buf
    endif ;;
drop