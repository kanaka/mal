Object subclass: Printer [
    Printer class >> prStr: sexp printReadably: printReadably [
        sexp type = #fn ifTrue: [ ^'#<fn>' ].
        sexp type = #func ifTrue: [ ^'#<func>' ].
        sexp type = #true ifTrue: [ ^'true' ].
        sexp type = #false ifTrue: [ ^'false' ].
        sexp type = #nil ifTrue: [ ^'nil' ].

        sexp type = #number ifTrue: [ ^sexp value asString ].
        sexp type = #symbol ifTrue: [ ^sexp value asString ].
        sexp type = #keyword ifTrue: [ ^':', sexp value ].

        sexp type = #string ifTrue: [
            printReadably ifTrue: [
                ^sexp value repr
            ] ifFalse: [
                ^sexp value
            ]
        ].

        sexp type = #list ifTrue: [
            ^self prList: sexp printReadably: printReadably
                  starter: '(' ender: ')'
        ].
        sexp type = #vector ifTrue: [
            ^self prList: sexp printReadably: printReadably
                  starter: '[' ender: ']'
        ].
        sexp type = #map ifTrue: [
            ^self prMap: sexp printReadably: printReadably
        ].

        sexp type = #atom ifTrue: [
            ^'(atom ', (self prStr: sexp value printReadably: printReadably), ')'
        ].

        Error halt: 'unimplemented type'
    ]

    Printer class >> prList: sexp printReadably: printReadably
            starter: starter ender: ender [
        | items |
        items := sexp value collect:
            [ :item | self prStr: item printReadably: printReadably ].
        ^starter, (items join: ' ') , ender
    ]

    Printer class >> prMap: sexp printReadably: printReadably [
        | items |
        items := sexp value associations collect:
            [ :item |
                (self prStr: item key printReadably: printReadably), ' ',
                    (self prStr: item value printReadably: printReadably) ].
        ^'{', (items join: ' '), '}'
    ]
]
