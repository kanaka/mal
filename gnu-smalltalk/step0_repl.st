String extend [
    String >> loadRelative [
        | scriptPath scriptDirectory |
        scriptPath := thisContext currentFileName.
        scriptDirectory := FilePath stripFileNameFor: scriptPath.
        FileStream fileIn: (FilePath append: self to: scriptDirectory)
    ]
]

'readline.st' loadRelative.

Object subclass: MAL [
    MAL class >> READ: input [
        ^input
    ]

    MAL class >> EVAL: sexp [
        ^sexp
    ]

    MAL class >> PRINT: sexp [
        ^sexp
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
        (MAL rep: input) displayNl.
    ]
]

'' displayNl.
