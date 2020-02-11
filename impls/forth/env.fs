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

: env/get-addr { key env -- val-addr }
    env
    begin ( env )
        key over MalEnv/data @ MalMap/get-addr ( env addr-or-0 )
        ?dup 0= if ( env )
            MalEnv/outer @ dup 0= ( env-or-0 done-looping? )
        else ( env addr )
            nip -1 \ found it! ( addr -1 )
        endif
    until ;

MalEnv
  extend pr-buf { env }
    env MalEnv/data @ pr-buf
    a-space s" outer: " str-append
    env MalEnv/outer @ ?dup 0= if
        s" <none>" str-append
    else
        pr-buf
    endif ;;
drop
