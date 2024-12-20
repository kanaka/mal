String extend [
    String >> loadRelative [
        | scriptPath scriptDirectory |
        scriptPath := thisContext currentFileName.
        scriptDirectory := FilePath stripFileNameFor: scriptPath.
        FileStream fileIn: (FilePath append: self to: scriptDirectory)
    ]
]

'readline.st' loadRelative.
'util.st' loadRelative.
'types.st' loadRelative.
'reader.st' loadRelative.
'printer.st' loadRelative.
'env.st' loadRelative.
'func.st' loadRelative.
'core.st' loadRelative.

Object subclass: MAL [
    MAL class >> READ: input [
        ^Reader readStr: input
    ]

    MAL class >> evalList: list env: env [
        ^list collect:
            [ :item | self EVAL: item env: env ].
    ]

    MAL class >> EVAL: aSexp env: anEnv [
        | sexp env ast a0_ a1 a1_ a2 a3 forms function args |

        "NOTE: redefinition of method arguments is not allowed"
        sexp := aSexp.
        env := anEnv.

        [
            [ :continue |

                a1 := env get: #'DEBUG-EVAL'.
                (a1 isNil or: [ a1 type = #false or: [ a1 type = #nil ] ] )
                ifFalse: [
                    ('EVAL: ' , (Printer prStr: sexp printReadably: true))
                    displayNl.
                ].

                sexp type = #symbol ifTrue: [
                    | key value |
                    key := sexp value.
                    value := env get: key.
                    value isNil ifTrue: [
                        ^MALUnknownSymbol new signal: key
                    ].
                    ^value
                ].
                sexp type = #vector ifTrue: [
                    ^MALVector new: (self evalList: sexp value env: env)
                ].
                sexp type = #map ifTrue: [
                    ^MALMap new: (self evalList: sexp value env: env)
                ].
                sexp type ~= #list ifTrue: [
                    ^sexp
                ].
                sexp value isEmpty ifTrue: [
                    ^sexp
                ].

                ast := sexp value.
                a0_ := ast first value.

                a0_ = #'def!' ifTrue: [
                    | result |
                    a1_ := ast second value.
                    a2 := ast third.
                    result := self EVAL: a2 env: env.
                    env set: a1_ value: result.
                    ^result
                ].

                a0_ = #'let*' ifTrue: [
                    | env_ |
                    env_ := Env new: env.
                    a1_ := ast second value.
                    a2 := ast third.
                    1 to: a1_ size by: 2 do:
                        [ :i | env_ set: (a1_ at: i) value
                                    value: (self EVAL: (a1_ at: i + 1)
                                                 env: env_) ].
                    env := env_.
                    sexp := a2.
                    continue value "TCO"
                ].

                a0_ = #do ifTrue: [
                    | forms last |
                    ast size < 2 ifTrue: [
                        forms := {}.
                        last := MALObject Nil.
                    ] ifFalse: [
                        forms := ast copyFrom: 2 to: ast size - 1.
                        last := ast last.
                    ].

                    forms do: [ :form | self EVAL: form env: env ].
                    sexp := last.
                    continue value "TCO"
                ].

                a0_ = #if ifTrue: [
                    | condition |
                    a1 := ast second.
                    a2 := ast third.
                    a3 := ast at: 4 ifAbsent: [ MALObject Nil ].
                    condition := self EVAL: a1 env: env.

                    (condition type = #false or:
                     [ condition type = #nil ]) ifTrue: [
                        sexp := a3
                    ] ifFalse: [
                        sexp := a2
                    ].
                    continue value "TCO"
                ].

                a0_ = #'fn*' ifTrue: [
                    | binds env_ fn |
                    a1_ := ast second value.
                    binds := a1_ collect: [ :item | item value ].
                    a2 := ast third.
                    fn := [ :args |
                        self EVAL: a2 env:
                            (Env new: env binds: binds exprs: args) ].
                    ^Func new: a2 params: binds env: env fn: fn
                ].

                forms := self evalList: sexp value env: env.
                function := forms first.
                args := forms allButFirst asArray.

                function type = #fn ifTrue: [ ^function fn value: args ].
                function type = #func ifTrue: [
                    | env_ |
                    sexp := function ast.
                    env_ := Env new: function env binds: function params
                                exprs: args.
                    env := env_.
                    continue value "TCO"
                ]
            ] valueWithExit
        ] repeat.
    ]

    MAL class >> PRINT: sexp [
        ^Printer prStr: sexp printReadably: true
    ]

    MAL class >> rep: input env: env [
        ^self PRINT: (self EVAL: (self READ: input) env: env)
    ]
]

| input historyFile replEnv |

historyFile := '.mal_history'.
ReadLine readHistory: historyFile.
replEnv := Env new: nil.

Core Ns keysAndValuesDo: [ :op :block | replEnv set: op value: block ].

MAL rep: '(def! not (fn* (a) (if a false true)))' env: replEnv.

[ input := ReadLine readLine: 'user> '. input isNil ] whileFalse: [
    input isEmpty ifFalse: [
        ReadLine addHistory: input.
        ReadLine writeHistory: historyFile.
        [ (MAL rep: input env: replEnv) displayNl ]
            on: MALEmptyInput do: [ #return ]
            on: MALError do:
                [ :err | ('error: ', err messageText) displayNl. #return ].
    ]
]

'' displayNl.
