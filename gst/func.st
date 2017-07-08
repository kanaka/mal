Object subclass: Func [
    | ast params env fn isMacro meta |

    ast [ ^ast ]
    params [ ^params ]
    env [ ^env ]
    fn [ ^fn ]
    isMacro [ ^isMacro ]
    meta [ ^meta ]

    isMacro: bool [
        isMacro := bool
    ]

    meta: aMeta [
        meta := aMeta
    ]

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
        isMacro := false
    ]

    withMeta: meta [
        | func |
        func := self deepCopy.
        func meta: meta.
        ^func
    ]
]
