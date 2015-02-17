require env.fs

0 MalEnv. constant core

: args-as-native { argv argc -- entry*argc... }
    argc 0 ?do
        argv i cells + @ as-native
    loop ;

: defcore* ( sym xt )
    MalNativeFn. core env/set ;

: defcore
    parse-allot-name MalSymbol. ( xt )
    ['] defcore* :noname ;

defcore +  args-as-native +  MalInt. ;;
defcore -  args-as-native -  MalInt. ;;
defcore *  args-as-native *  MalInt. ;;
defcore /  args-as-native /  MalInt. ;;
defcore <  args-as-native <  mal-bool ;;
defcore >  args-as-native >  mal-bool ;;
defcore <= args-as-native <= mal-bool ;;
defcore >= args-as-native >= mal-bool ;;

defcore list { argv argc }
    MalList new { list }
    argc cells allocate throw { start }
    argv  start  argc cells  cmove
    argc  list MalList/count !
    start list MalList/start !
    list ;;

defcore list? drop @ mal-type @ MalList = mal-bool ;;
defcore empty? drop @ empty? ;;
defcore count drop @ mal-count ;;

defcore = drop dup @ swap cell+ @ swap m= mal-bool ;;
defcore not
    drop @
    dup mal-nil = if
        drop mal-true
    else
        mal-false = if
            mal-true
        else
            mal-false
        endif
    endif ;;

: pr-str-multi ( readably? argv argc )
    ?dup 0= if drop 0 0
    else
        { argv argc }
        new-str
        argv @ pr-buf
        argc 1 ?do
            a-space
            argv i cells + @ pr-buf
        loop
    endif ;

defcore prn true -rot pr-str-multi type cr drop mal-nil ;;
defcore pr-str true -rot pr-str-multi MalString. nip ;;
defcore println false -rot pr-str-multi type cr drop mal-nil ;;
defcore str ( argv argc )
    dup 0= if
        MalString.
    else
        { argv argc }
        false new-str
        argc 0 ?do
            argv i cells + @ pr-buf
        loop
        MalString. nip
    endif ;;

defcore read-string drop @ unpack-str read-str ;;
defcore slurp drop @ unpack-str slurp-file MalString. ;;

defcore cons ( argv[item,coll] argc )
    drop dup @ swap cell+ @ ( item coll )
    to-list conj ;;

defcore concat { lists argc }
    0   lists argc cells +  lists  +do ( count )
        i @ to-list MalList/count @ +
    cell +loop { count }
    count cells allocate throw { start }
    start   lists argc cells +  lists  +do ( target )
        i @ to-list MalList/count @ cells  2dup  i @ to-list MalList/start @  -rot  ( target bytes src target bytes )
        cmove ( target bytes )
        + ( new-target )
    cell +loop
    drop
    MalList new
    start over MalList/start !
    count over MalList/count ! ;;

defcore nth ( argv[coll,i] argc )
    over ( argv argc argv )
    cell+ @ MalInt/int @ ( argv argc count )
    swap over <= if ." nth out of bounds" cr 1 throw endif ( argv count )
    cells swap ( c-offset argv )
    @ to-list MalList/start @ + @ ;;

defcore first ( argv[coll] argc )
    drop @ to-list
    dup MalList/count @ 0= if
        drop mal-nil
    else
        MalList/start @ @
    endif ;;

defcore rest ( argv[coll] argc )
    drop @ to-list MalList/rest ;;
