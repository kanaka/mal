require env.fs

0 MalEnv. constant core

: args-as-native drop { argv argc -- entry*argc... }
    argc 0 ?do
        argv i cells + @ as-native
    loop ;

: defcore ( xt )
    parse-allot-name MalSymbol. ( xt sym )
    swap MalFn. core env/set ;

:noname args-as-native + MalInt. ; defcore +
:noname args-as-native - MalInt. ; defcore -
:noname args-as-native * MalInt. ; defcore *
:noname args-as-native / MalInt. ; defcore /
:noname args-as-native < mal-bool ; defcore <
:noname args-as-native > mal-bool ; defcore >
:noname args-as-native <= mal-bool ; defcore <=
:noname args-as-native >= mal-bool ; defcore >=

:noname drop { argv argc }
    MalList new { list }
    argc cells allocate throw { start }
    argv  start  argc cells  cmove
    argc  list MalList/count !
    start list MalList/start !
    list
; defcore list

:noname 2drop @ mal-type @ MalList = mal-bool ; defcore list?
:noname 2drop @ empty? ; defcore empty?
:noname 2drop @ mal-count ; defcore count

:noname 2drop dup @ swap cell+ @ swap m= mal-bool ; defcore =
