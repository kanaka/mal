FileStream fileIn: 'types.st'.

Object subclass: Env [
    | data outer |

    Env class >> new: outerEnv [
        | env |
        env := super new.
        env init: outerEnv.
        ^env
    ]

    init: env [
        data := Dictionary new.
        outer := env.
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
