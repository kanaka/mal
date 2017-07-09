Object subclass: Env [
    | data outer |

    Env class >> new: outerEnv [
        ^self new: outerEnv binds: {} exprs: {}
    ]

    Env class >> new: outerEnv binds: binds exprs: exprs [
        | env |
        env := super new.
        env init: outerEnv binds: binds exprs: exprs.
        ^env
    ]

    init: env binds: binds exprs: exprs [
        data := Dictionary new.
        outer := env.
        1 to: binds size do:
            [ :i | (binds at: i) = #& ifTrue: [
                | rest |
                rest := OrderedCollection from: (exprs copyFrom: i).
                self set: (binds at: i + 1) value: (MALList new: rest).
                ^nil
            ] ifFalse: [
                self set: (binds at: i) value: (exprs at: i)
            ] ]
    ]

    set: key value: value [
        data at: key put: value.
    ]

    find: key [
        ^data at: key ifAbsent: [
            outer notNil ifTrue: [
                outer find: key
            ] ifFalse: [
                nil
            ]
        ]
    ]

    get: key [
        | value |
        value := self find: key.

        value notNil ifTrue: [
            ^value
        ] ifFalse: [
            ^MALUnknownSymbol new signal: key
        ]
    ]
]
