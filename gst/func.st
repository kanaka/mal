Object subclass: Func [
    | ast params env fn meta |

    ast [ ^ast ]
    params [ ^params ]
    env [ ^env ]
    fn [ ^fn ]
    meta [ ^meta ]

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
    ]

    withMeta: meta [
        | func |
        func := self deepCopy.
        func meta: meta.
        ^func
    ]
]
