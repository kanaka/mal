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
    (Fn new: [ :args | MALNumber new: args first value + args second value ]).
Core Ns at: #- put:
    (Fn new: [ :args | MALNumber new: args first value - args second value ]).
Core Ns at: #* put:
    (Fn new: [ :args | MALNumber new: args first value * args second value ]).
Core Ns at: #/ put:
    (Fn new: [ :args | MALNumber new: args first value // args second value ]).

Core Ns at: #'pr-str' put:
    (Fn new: [ :args | MALString new: (Core printedArgs: args readable: true
                                            sep: ' ') ]).
Core Ns at: #str put:
    (Fn new: [ :args | MALString new: (Core printedArgs: args readable: false
                                           sep: '') ]).
Core Ns at: #prn put:
    (Fn new: [ :args |
        (Core printedArgs: args readable: true sep: ' ') displayNl.
        MALObject Nil ]).
Core Ns at: #println put:
    (Fn new: [ :args |
        (Core printedArgs: args readable: false sep: ' ') displayNl.
        MALObject Nil ]).

Core Ns at: #list put:
    (Fn new: [ :args | MALList new: (OrderedCollection from: args) ]).
Core Ns at: #'list?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #list ] ]).
Core Ns at: #'empty?' put:
    (Fn new: [ :args | Core coerce: [ args first value isEmpty ] ]).
Core Ns at: #count put:
    (Fn new: [ :args | MALNumber new: args first value size ]).

Core Ns at: #= put:
    (Fn new: [ :args | Core coerce: [ args first = args second ] ]).

Core Ns at: #< put:
    (Fn new: [ :args | Core coerce: [ args first value < args second value ] ]).
Core Ns at: #<= put:
    (Fn new: [ :args | Core coerce: [ args first value <= args second value ] ]).
Core Ns at: #> put:
    (Fn new: [ :args | Core coerce: [ args first value > args second value ] ]).
Core Ns at: #>= put:
    (Fn new: [ :args | Core coerce: [ args first value >= args second value ] ]).

Core Ns at: #'read-string' put:
    (Fn new: [ :args | Reader readStr: args first value ]).
Core Ns at: #slurp put:
    (Fn new: [ :args | MALString new: (File path: args first value) contents ]).
Core Ns at: #throw put:
    (Fn new: [ :args | MALCustomError new signal: args first ]).
Core Ns at: #readline put:
    (Fn new: [ :args |
        | result |
        result := ReadLine readLine: args first value.
        result isString ifTrue: [
            MALString new: result
        ] ifFalse: [
            MALObject Nil
        ] ]).
Core Ns at: #'time-ms' put:
    (Fn new: [ :args | MALNumber new: Time millisecondClock ]).
Core Ns at: #'gst-eval' put:
    (Fn new: [ :args | (Behavior evaluate: args first value) toMALValue ]).

Core Ns at: #atom put:
    (Fn new: [ :args | MALAtom new: args first ]).
Core Ns at: #'atom?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #atom ] ]).
Core Ns at: #deref put:
    (Fn new: [ :args | args first value ]).
Core Ns at: #'reset!' put:
    (Fn new: [ :args | args first value: args second. args second ]).
Core Ns at: #'swap!' put:
    (Fn new: [ :args |
        | a f x xs result |
        a := args first.
        f := args second fn.
        x := a value.
        xs := args allButFirst: 2.
        result := f value: (xs copyWithFirst: x).
        a value: result.
        result ]).

Core Ns at: #cons put:
    (Fn new: [ :args | MALList new: (args second value copyWithFirst: args first) ]).
Core Ns at: #concat put:
    (Fn new: [ :args | MALList new: (OrderedCollection join:
        (args collect: [ :arg | arg value ])) ]).
Core Ns at: #nth put:
    (Fn new: [ :args |
        | items index |
        items := args first value.
        index := args second value + 1.
        items at: index ifAbsent: [ MALOutOfBounds new signal ] ]).
Core Ns at: #first put:
    (Fn new: [ :args | Core nilable: args else: [
        args first value at: 1 ifAbsent: [ MALObject Nil ] ] ]).
Core Ns at: #rest put:
    (Fn new: [ :args |
        | items rest |
        items := args first value.
        (args first type = #nil or: [ items isEmpty  ]) ifTrue: [
            rest := {}
        ] ifFalse: [
            rest := items allButFirst
        ].
        MALList new: (OrderedCollection from: rest) ]).
Core Ns at: #conj put:
    (Fn new: [ :args |
        | kind result items |
        kind := args first type.
        result := args first value.
        items := args allButFirst.

        kind = #list ifTrue: [
            MALList new: (OrderedCollection from: items reverse, result)
        ] ifFalse: [
            MALVector new: (OrderedCollection from: result, items)
        ] ]).
Core Ns at: #seq put:
    (Fn new: [ :args |
        | kind storage result |
        kind := args first type.
        storage := args first value.
        Core nilable: args else: [
            storage isEmpty ifTrue: [
                MALObject Nil
            ] ifFalse: [
               kind = #string ifTrue: [
                   result := (OrderedCollection from: storage) collect:
                       [ :char | MALString new: char asString ].
                   MALList new: result
               ] ifFalse: [
                   MALList new: (OrderedCollection from: storage)
               ]
            ]
        ] ]).

Core Ns at: #apply put:
    (Fn new: [ :args |
        | f rest result |
        f := args first fn.
        args size < 3 ifTrue: [
            rest := {}
        ] ifFalse: [
            rest := args copyFrom: 2 to: args size - 1
        ].
        rest := rest, args last value.
        f value: rest ]).
Core Ns at: #map put:
    (Fn new: [ :args |
        | items f result |
        f := args first fn.
        items := args second value.
        result := items collect: [ :item | f value: {item} ].
        MALList new: (OrderedCollection from: result) ]).

Core Ns at: #meta put:
    (Fn new: [ :args |
        | meta |
        meta := args first meta.
        meta isNil ifTrue: [ MALObject Nil ] ifFalse: [ meta ] ]).
Core Ns at: #'with-meta' put:
    (Fn new: [ :args | args first withMeta: args second ]).

Core Ns at: #'nil?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #nil ] ]).
Core Ns at: #'true?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #true ] ]).
Core Ns at: #'false?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #false ] ]).
Core Ns at: #'number?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #number ] ]).
Core Ns at: #'symbol?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #symbol ] ]).
Core Ns at: #'keyword?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #keyword ] ]).
Core Ns at: #'string?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #string ] ]).
Core Ns at: #'vector?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #vector ] ]).
Core Ns at: #'map?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #map ] ]).
Core Ns at: #'sequential?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #list or:
                                     [ args first type = #vector ] ] ]).
Core Ns at: #'fn?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #fn or:
                                     [ args first type = #func and:
                                      [ args first isMacro not ] ] ] ]).
Core Ns at: #'macro?' put:
    (Fn new: [ :args | Core coerce: [ args first type = #func and:
                                     [ args first isMacro ] ] ]).

Core Ns at: #symbol put:
    (Fn new: [ :args | MALSymbol new: args first value asSymbol ]).
Core Ns at: #keyword put:
    (Fn new: [ :args | MALKeyword new: args first value asSymbol ]).
Core Ns at: #'vec' put:
    (Fn new: [ :args | MALVector new: args first value ]).
Core Ns at: #vector put:
    (Fn new: [ :args | MALVector new: (OrderedCollection from: args) ]).
Core Ns at: #'hash-map' put:
    (Fn new: [ :args | MALMap new: args asDictionary ]).

Core Ns at: #assoc put:
    (Fn new: [ :args |
        | result keyVals |
        result := Dictionary from: args first value associations.
        keyVals := args allButFirst.
        1 to: keyVals size by: 2 do:
            [ :i | result add: (keyVals at: i) -> (keyVals at: i + 1) ].
        MALMap new: result ]).
Core Ns at: #dissoc put:
    (Fn new: [ :args |
        | result keys |
        result := Dictionary from: args first value associations.
        keys := args allButFirst.
        keys do: [ :key | result removeKey: key ifAbsent: [ nil ] ].
        MALMap new: result ]).
Core Ns at: #get put:
    (Fn new: [ :args | Core nilable: args else:
        [ args first value at: args second ifAbsent: [ MALObject Nil ] ] ]).
Core Ns at: #'contains?' put:
    (Fn new: [ :args | Core coerce: [ args first value includesKey: args second ] ]).
Core Ns at: #keys put:
    (Fn new: [ :args | MALList new: (OrderedCollection from: args first value keys) ]).
Core Ns at: #vals put:
    (Fn new: [ :args | MALList new: (OrderedCollection from: args first value values) ]).
