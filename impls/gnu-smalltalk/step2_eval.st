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

    MAL class >> evalList: list env: env [
        ^list collect:
            [ :item | self EVAL: item env: env ].
    ]

    MAL class >> EVAL: sexp env: env [
        | forms function args |

        " ('EVAL: ' , (Printer prStr: sexp printReadably: true)) displayNl. "

        sexp type = #symbol ifTrue: [
            ^env at: sexp value ifAbsent: [
                ^MALUnknownSymbol new signal: sexp value
            ].
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
