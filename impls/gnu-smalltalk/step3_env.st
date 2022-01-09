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

Object subclass: MAL [
    MAL class >> READ: input [
        ^Reader readStr: input
    ]

    MAL class >> evalList: list env: env [
        ^list collect:
            [ :item | self EVAL: item env: env ].
    ]

    MAL class >> EVAL: sexp env: env [
        | ast a0_ a1 a1_ a2 forms function args |

        a2 := env get: #'DEBUG-EVAL'.
        (a2 isNil or: [ a2 type = #false or: [ a2 type = #nil ] ] )
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
                            value: (self EVAL: (a1_ at: i + 1) env: env_) ].
            ^self EVAL: a2 env: env_
        ].

        forms := self evalList: sexp value env: env.
        function := forms first.
        args := forms allButFirst asArray.
        ^function valueWithArguments: args
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

replEnv set: #+ value: [ :a :b | MALNumber new: a value + b value ].
replEnv set: #- value: [ :a :b | MALNumber new: a value - b value ].
replEnv set: #* value: [ :a :b | MALNumber new: a value * b value ].
replEnv set: #/ value: [ :a :b | MALNumber new: a value // b value ].

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
