MALObject subclass: Func [
    | ast params env fn isMacro |

    ast [ ^ast ]
    params [ ^params ]
    env [ ^env ]
    fn [ ^fn ]
    isMacro [ ^isMacro ]

    isMacro: bool [
        isMacro := bool
    ]

    Func class >> new: ast params: params env: env fn: fn [
        | func |
        func := super new: #func value: fn meta: nil.
        func init: ast params: params env: env fn: fn.
        ^func
    ]

    init: anAst params: someParams env: anEnv fn: aFn [
        ast := anAst.
        params := someParams.
        env := anEnv.
        fn := aFn.
        isMacro := false
    ]
]
