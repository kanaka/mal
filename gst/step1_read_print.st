FileStream fileIn: 'readline.st'.
FileStream fileIn: 'reader.st'.
FileStream fileIn: 'printer.st'.

Object subclass: MAL [
    MAL class >> READ: input [
        ^Reader readStr: input
    ]

    MAL class >> EVAL: sexp [
        ^sexp
    ]

    MAL class >> PRINT: sexp [
        ^Printer prStr: sexp printReadably: true
    ]

    MAL class >> rep: input [
        ^self PRINT: (self EVAL: (self READ: input))
    ]
]

| input historyFile |

historyFile := '.mal_history'.
ReadLine readHistory: historyFile.

[ input := ReadLine readLine: 'user> '. input isNil ] whileFalse: [
    input isEmpty ifFalse: [
        ReadLine addHistory: input.
        ReadLine writeHistory: historyFile.
        [ (MAL rep: input) displayNl ]
            on: MALEmptyInput do: [ #return ]
            on: MALError do:
                [ :err | ('error: ', err messageText) displayNl. #return ].
    ]
]

'' displayNl.
