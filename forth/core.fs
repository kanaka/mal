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

: pr-str-multi ( argv argc )
    ?dup 0= if drop s" "
    else
        { argv argc }
        new-str
        argv @ pr-buf
        argc 1 ?do
            a-space
            argv i cells + @ pr-buf
        loop
    endif ;

defcore prn pr-str-multi type cr mal-nil ;;
defcore pr-str pr-str-multi MalString. ;;

defcore str drop @ pr-str MalString. ;;
defcore println pr-str-multi 10 str-append-char MalString. ;;
