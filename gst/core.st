FileStream fileIn: 'types.st'.
FileStream fileIn: 'printer.st'.
FileStream fileIn: 'reader.st'.

Object subclass: Core [
    Ns := Dictionary new.
    Core class >> Ns [ ^Ns ]

    Core class >> coerce: block [
        block value ifTrue: [ ^MALObject True ] ifFalse: [ ^MALObject False ]
    ]

    Core class >> nilable: args else: block [
        args first type = #nil ifTrue: [
            ^MALObject Nil
        ] ifFalse: [
            ^block value
        ]
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
Core Ns at: #throw put:
    [ :args | MALCustomError new signal: args first ].

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

Core Ns at: #cons put:
    [ :args | MALList new: (args second value copyWithFirst: args first) ].
Core Ns at: #concat put:
    [ :args | MALList new: (OrderedCollection join:
        (args collect: [ :arg | arg value ])) ].
Core Ns at: #nth put:
    [ :args |
        | items index |
        items := args first value.
        index := args second value + 1.
        items at: index ifAbsent: [ MALOutOfBounds new signal ]
    ].
Core Ns at: #first put:
    [ :args | Core nilable: args else: [
        args first value at: 1 ifAbsent: [ MALObject Nil ] ] ].
Core Ns at: #rest put:
    [ :args |
        | items rest |
        items := args first value.
        (args first type = #nil or: [ items isEmpty  ]) ifTrue: [
            rest := {}
        ] ifFalse: [
            rest := items allButFirst
        ].
        MALList new: (OrderedCollection from: rest)
    ].

Core Ns at: #apply put:
    [ :args |
        | f rest result |
        f := args first.
        f class = Func ifTrue: [ f := f fn ].
        args size < 3 ifTrue: [
            rest := {}
        ] ifFalse: [
            rest := args copyFrom: 2 to: args size - 1
        ].
        rest := rest, args last value.
        f value: rest
    ].
Core Ns at: #map put:
    [ :args |
        | items f result |
        f := args first.
        f class = Func ifTrue: [ f := f fn ].
        items := args second value.
        result := items collect: [ :item | f value: {item} ].
        MALList new: (OrderedCollection from: result)
    ].

Core Ns at: #'nil?' put:
    [ :args | Core coerce: [ args first type = #nil ] ].
Core Ns at: #'true?' put:
    [ :args | Core coerce: [ args first type = #true ] ].
Core Ns at: #'false?' put:
    [ :args | Core coerce: [ args first type = #false ] ].
Core Ns at: #'symbol?' put:
    [ :args | Core coerce: [ args first type = #symbol ] ].
Core Ns at: #'keyword?' put:
    [ :args | Core coerce: [ args first type = #keyword ] ].
Core Ns at: #'vector?' put:
    [ :args | Core coerce: [ args first type = #vector ] ].
Core Ns at: #'map?' put:
    [ :args | Core coerce: [ args first type = #map ] ].
Core Ns at: #'sequential?' put:
    [ :args | Core coerce: [ args first type = #list or:
                            [ args first type = #vector ] ] ].

Core Ns at: #symbol put:
    [ :args | MALSymbol new: args first value asSymbol ].
Core Ns at: #keyword put:
    [ :args | MALKeyword new: args first value asSymbol ].
Core Ns at: #vector put:
    [ :args | MALVector new: (OrderedCollection from: args) ].
Core Ns at: #'hash-map' put:
    [ :args | MALMap new: args asDictionary ].

Core Ns at: #assoc put:
    [ :args |
        | result keyVals |
        result := Dictionary from: args first value associations.
        keyVals := args allButFirst.
        1 to: keyVals size by: 2 do:
            [ :i | result add: (keyVals at: i) -> (keyVals at: i + 1) ].
        MALMap new: result
    ].
Core Ns at: #dissoc put:
    [ :args |
        | result keys |
        result := Dictionary from: args first value associations.
        keys := args allButFirst.
        keys do: [ :key | result removeKey: key ifAbsent: [ nil ] ].
        MALMap new: result
    ].
Core Ns at: #get put:
    [ :args | Core nilable: args else:
        [ args first value at: args second ifAbsent: [ MALObject Nil ] ] ].
Core Ns at: #'contains?' put:
    [ :args | Core coerce: [ args first value includesKey: args second ] ].
Core Ns at: #keys put:
    [ :args | MALList new: (OrderedCollection from: args first value keys) ].
Core Ns at: #vals put:
    [ :args | MALList new: (OrderedCollection from: args first value values) ].
