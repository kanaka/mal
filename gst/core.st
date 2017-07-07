FileStream fileIn: 'types.st'.
FileStream fileIn: 'printer.st'.
FileStream fileIn: 'reader.st'.

Object subclass: Core [
    Ns := Dictionary new.
    Core class >> Ns [ ^Ns ]

    Core class >> coerce: block [
        block value ifTrue: [ ^MALObject True ] ifFalse: [ ^MALObject False ]
    ]

    Core class >> printedArgs: args readable: readable sep: sep [
        | items |
        items := args collect:
            [ :arg | Printer prStr: arg printReadably: readable ].
        "NOTE: {} join returns the unchanged array"
        items isEmpty ifTrue: [ ^'' ] ifFalse: [ ^items join: sep ]
    ]
]

Core Ns at: #+ put:
    [ :args | MALNumber new: args first value + args second value ].
Core Ns at: #- put:
    [ :args | MALNumber new: args first value - args second value ].
Core Ns at: #* put:
    [ :args | MALNumber new: args first value * args second value ].
Core Ns at: #/ put:
    [ :args | MALNumber new: args first value // args second value ].

Core Ns at: #'pr-str' put:
    [ :args | MALString new: (Core printedArgs: args readable: true sep: ' ') ].
Core Ns at: #str put:
    [ :args | MALString new: (Core printedArgs: args readable: false sep: '') ].
Core Ns at: #prn put:
    [ :args | (Core printedArgs: args readable: true sep: ' ') displayNl.
        MALObject Nil ].
Core Ns at: #println put:
    [ :args | (Core printedArgs: args readable: false sep: ' ') displayNl.
        MALObject Nil ].

Core Ns at: #list put: [ :args | MALList new: (OrderedCollection from: args) ].
Core Ns at: #'list?' put:
    [ :args | Core coerce: [ args first type = #list ] ].
Core Ns at: #'empty?' put:
    [ :args | Core coerce: [ args first value isEmpty ] ].
Core Ns at: #count put:
    [ :args | MALNumber new: args first value size ].

Core Ns at: #= put:
    [ :args | Core coerce: [ args first = args second ] ].

Core Ns at: #< put:
    [ :args | Core coerce: [ args first value < args second value ] ].
Core Ns at: #<= put:
    [ :args | Core coerce: [ args first value <= args second value ] ].
Core Ns at: #> put:
    [ :args | Core coerce: [ args first value > args second value ] ].
Core Ns at: #>= put:
    [ :args | Core coerce: [ args first value >= args second value ] ].

Core Ns at: #'read-string' put:
    [ :args | Reader readStr: args first value ].
Core Ns at: #slurp put:
    [ :args | MALString new: (File path: args first value) contents ].

Core Ns at: #atom put:
    [ :args | MALAtom new: args first ].
Core Ns at: #'atom?' put:
    [ :args | Core coerce: [ args first type = #atom ] ].
Core Ns at: #deref put:
    [ :args | args first value ].
Core Ns at: #'reset!' put:
    [ :args | args first value: args second. args second ].
Core Ns at: #'swap!' put:
    [ :args |
        | a f x xs result |
        a := args first.
        f := args second.
        f class = Func ifTrue: [ f := f fn ].
        x := a value.
        xs := args allButFirst: 2.
        result := f value: (xs copyWithFirst: x).
        a value: result.
        result
    ].
