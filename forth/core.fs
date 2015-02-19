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
    argc cells allocate throw { start }
    argv  start  argc cells  cmove
    start argc MalList. ;;

defcore vector { argv argc }
    argc cells allocate throw { start }
    argv  start  argc cells  cmove
    start argc MalList.
    MalVector new swap over MalVector/list ! ;;

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
    MalList new
    lists over MalList/start !
    argc over MalList/count !
    MalList/concat ;;

defcore conj { argv argc }
    argv @ ( coll )
    argc 1 ?do
        argv i cells + @ swap conj
    loop ;;

defcore assoc { argv argc }
    argv @ ( coll )
    argv argc cells +  argv cell+  +do
        i @ \ key
        i cell+ @ \ val
        rot assoc
    2 cells +loop ;;

defcore keys ( argv argc )
    drop @ MalMap/list @
    dup MalList/start @ swap MalList/count @ { start count }
    here
    start count cells +  start  +do
        i @ ,
    2 cells +loop
    here>MalList ;;

defcore vals ( argv argc )
    drop @ MalMap/list @
    dup MalList/start @ swap MalList/count @ { start count }
    here
    start count cells +  start cell+  +do
        i @ ,
    2 cells +loop
    here>MalList ;;

defcore dissoc { argv argc }
    argv @ \ coll
    argv argc cells +  argv cell+  +do
        i @ swap dissoc
    cell +loop ;;

defcore hash-map { argv argc }
    MalMap/Empty
    argc cells argv +  argv  +do
        i @  i cell+ @  rot  assoc
    2 cells +loop ;;

defcore get { argv argc }
    argc 3 < if mal-nil else argv cell+ cell+ @ endif
    argv cell+ @ \ key
    argv @ \ coll
    get ;;

defcore contains? { argv argc }
    0
    argv cell+ @ \ key
    argv @ \ coll
    get 0 <> mal-bool ;;

defcore nth ( argv[coll,i] argc )
    drop dup @ to-list ( argv list )
    swap cell+ @ MalInt/int @ ( list i )
    over MalList/count @ ( list i count )
    2dup >= if { i count }
        0 0
        new-str i int>str str-append s\" \040>= " count int>str
        s" nth out of bounds: " ...throw-str
    endif drop ( list i )
    cells swap ( c-offset list )
    MalList/start @ + @ ;;

defcore first ( argv[coll] argc )
    drop @ to-list
    dup MalList/count @ 0= if
        drop mal-nil
    else
        MalList/start @ @
    endif ;;

defcore rest ( argv[coll] argc )
    drop @ to-list MalList/rest ;;

defcore meta ( argv[obj] argc )
    drop @ mal-meta @
    ?dup 0= if mal-nil endif ;;

defcore with-meta ( argv[obj,meta] argc )
    drop ( argv )
    dup cell+ @  swap @  ( meta obj )
    dup mal-type @ MalTypeType-struct @ ( meta obj obj-size )
    dup allocate throw { new-obj } ( meta obj obj-size )
    new-obj swap cmove ( meta )
    new-obj mal-meta ! ( )
    new-obj ;;

defcore atom ( argv[val] argc )
    drop @ Atom. ;;

defcore deref ( argv[atom] argc )
    drop @ Atom/val @ ;;

defcore reset! ( argv[atom,val] argc )
    drop dup cell+ @ ( argv val )
    dup -rot swap @ Atom/val ! ;;

defcore apply { argv argc -- val }
    \ argv is (fn args... more-args)
    argv argc 1- cells + @ to-list { more-args }
    argc 2 - { list0len }
    more-args MalList/count @ list0len + { final-argc }
    final-argc cells allocate throw { final-argv }
    argv cell+   final-argv   list0len cells   cmove
    more-args MalList/start @   final-argv list0len cells +  final-argc list0len - cells  cmove
    final-argv final-argc argv @  invoke ;;

defcore throw ( argv argc -- )
    drop @ to exception-object
    1 throw ;;

defcore map?     drop @ mal-type @ MalMap     = mal-bool ;;
defcore list?    drop @ mal-type @ MalList    = mal-bool ;;
defcore vector?  drop @ mal-type @ MalVector  = mal-bool ;;
defcore keyword? drop @ mal-type @ MalKeyword = mal-bool ;;
defcore symbol?  drop @ mal-type @ MalSymbol  = mal-bool ;;
defcore atom?    drop @ mal-type @ Atom       = mal-bool ;;
defcore true?    drop @ mal-true  = mal-bool ;;
defcore false?   drop @ mal-false = mal-bool ;;
defcore nil?     drop @ mal-nil   = mal-bool ;;

defcore sequential? drop @ sequential? ;;

defcore keyword  drop @ unpack-str MalKeyword. ;;
defcore symbol   drop @ unpack-str MalSymbol. ;;

defcore time-ms 2drop utime d>s 1000 / MalInt. ;;
