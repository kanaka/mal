Object subclass: Func [
    | ast params env fn |

    ast [ ^ast ]
    params [ ^params ]
    env [ ^env ]
    fn [ ^fn ]

    Func class >> new: ast params: params env: env fn: fn [
        | func |
        func := super new.
        func init: ast params: params env: env fn: fn.
        ^func
    ]

    init: anAst params: someParams env: anEnv fn: aFn [
        ast := anAst.
        params := someParams.
        env := anEnv.
        fn := aFn.
    ]
]
