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

Object subclass: MAL [
    MAL class >> READ: input [
        ^Reader readStr: input
    ]

    MAL class >> evalAst: sexp env: env [
        sexp type = #symbol ifTrue: [
            ^env at: sexp value ifAbsent: [
                ^MALUnknownSymbol new signal: sexp value
            ].
        ].

        sexp type = #list ifTrue: [
            ^self evalList: sexp env: env class: MALList
        ].
        sexp type = #vector ifTrue: [
            ^self evalList: sexp env: env class: MALVector
        ].
        sexp type = #map ifTrue: [
            ^self evalList: sexp env: env class: MALMap
        ].

        ^sexp
    ]

    MAL class >> evalList: sexp env: env class: aClass [
        | items |
        items := sexp value collect:
            [ :item | self EVAL: item env: env ].
        ^aClass new: items
    ]

    MAL class >> EVAL: sexp env: env [
        | forms function args |
        sexp type ~= #list ifTrue: [
            ^self evalAst: sexp env: env
        ].
        sexp value isEmpty ifTrue: [
            ^sexp
        ].

        forms := (self evalAst: sexp env: env) value.
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
replEnv := Dictionary from:
    { #+ -> [ :a :b | MALNumber new: a value + b value ].
      #- -> [ :a :b | MALNumber new: a value - b value ].
      #* -> [ :a :b | MALNumber new: a value * b value ].
      #/ -> [ :a :b | MALNumber new: a value // b value ] }.

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
