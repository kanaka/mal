require env.fs

0 MalEnv. constant core

: args-as-native { argv argc -- entry*argc... }
    argc 0 ?do
        argv i cells + @ as-native
    loop ;

: defcore ( xt )
    parse-allot-name MalSymbol. ( xt sym )
    swap MalNativeFn. core env/set ;

:noname args-as-native + MalInt. ; defcore +
:noname args-as-native - MalInt. ; defcore -
:noname args-as-native * MalInt. ; defcore *
:noname args-as-native / MalInt. ; defcore /
:noname args-as-native < mal-bool ; defcore <
:noname args-as-native > mal-bool ; defcore >
:noname args-as-native <= mal-bool ; defcore <=
:noname args-as-native >= mal-bool ; defcore >=

:noname { argv argc }
    MalList new { list }
    argc cells allocate throw { start }
    argv  start  argc cells  cmove
    argc  list MalList/count !
    start list MalList/start !
    list
; defcore list

:noname drop @ mal-type @ MalList = mal-bool ; defcore list?
:noname drop @ empty? ; defcore empty?
:noname drop @ mal-count ; defcore count

:noname drop dup @ swap cell+ @ swap m= mal-bool ; defcore =
