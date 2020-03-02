Object subclass: Reader [
    | storage index |

    TokenRegex := '[\s,]*(~@|[\[\]{}()''`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}(''"`,;)]*)'.
    CommentRegex := ';.*'.
    NumberRegex := '-?[0-9]+(?:\.[0-9]+)?'.
    StringRegex := '"(?:\\.|[^\\"])*"'.

    Reader class >> tokenizer: input [
        | tokens token hit pos done |
        tokens := OrderedCollection new.
        pos := 1.
        done := false.

        [done] whileFalse: [
            hit := input searchRegex: TokenRegex startingAt: pos.
            token := hit at: 1.
            token size = 0 ifTrue: [
                tokens add: (input copyFrom: pos to: input size) trimSeparators.
                done := true.
            ].
            (token size = 0 or: [token matchRegex: CommentRegex]) ifFalse: [
                tokens add: token
            ].
            pos := pos + (hit match size).
            pos > input size ifTrue: [
                done := true.
            ].
        ].
        ^tokens
    ]

    Reader class >> readStr: input [
        | tokens reader form |
        tokens := self tokenizer: input.
        reader := self new: tokens.
        tokens isEmpty ifTrue: [
            ^MALEmptyInput new signal
        ].
        ^self readForm: reader.
    ]

    Reader class >> readForm: reader [
        | token |
        token := reader peek.
        token = '(' ifTrue: [
            ^self readList: reader class: MALList ender: ')'
        ].
        token = '[' ifTrue: [
            ^self readList: reader class: MALVector ender: ']'
        ].
        token = '{' ifTrue: [
            ^self readList: reader class: MALMap ender: '}'
        ].

        (token matchRegex: '[])}]') ifTrue: [
            ^MALUnexpectedToken new signal: token
        ].

        token = '''' ifTrue: [
            ^self readSimpleMacro: reader name: #quote
        ].
        token = '`' ifTrue: [
            ^self readSimpleMacro: reader name: #quasiquote
        ].
        token = '~' ifTrue: [
            ^self readSimpleMacro: reader name: #unquote
        ].
        token = '~@' ifTrue: [
            ^self readSimpleMacro: reader name: #'splice-unquote'
        ].
        token = '@' ifTrue: [
            ^self readSimpleMacro: reader name: #deref
        ].

        token = '^' ifTrue: [
            ^self readWithMetaMacro: reader
        ].

        ^self readAtom: reader
    ]

    Reader class >> readList: reader class: aClass ender: ender [
        | storage token |
        storage := OrderedCollection new.
        "pop opening token"
        reader next.
        [ token := reader peek. token isNil ] whileFalse: [
            token = ender ifTrue: [
                ender = '}' ifTrue: [
                    storage := storage asDictionary.
                ].
                "pop closing token"
                reader next.
                ^aClass new: storage
            ].
            storage add: (self readForm: reader).
        ].
        ^MALUnterminatedSequence new signal: ender
    ]

    Reader class >> readAtom: reader [
        | token |
        token := reader next.

        token = 'true' ifTrue: [ ^MALObject True ].
        token = 'false' ifTrue: [ ^MALObject False ].
        token = 'nil' ifTrue: [ ^MALObject Nil ].

        (token matchRegex: StringRegex) ifTrue: [
            ^MALString new: token parse
        ].
        (token first = $") ifTrue: [
            ^MALUnterminatedSequence new signal: '"'
        ].

        (token matchRegex: NumberRegex) ifTrue: [
            ^MALNumber new: token asNumber
        ].

        (token first = $:) ifTrue: [
            ^MALKeyword new: token allButFirst asSymbol
        ].

        ^MALSymbol new: token asSymbol
    ]

    Reader class >> readSimpleMacro: reader name: name [
        | form list |
        "pop reader macro token"
        reader next.
        form := self readForm: reader.
        list := OrderedCollection from: { MALSymbol new: name. form }.
        ^MALList new: list
    ]

    Reader class >> readWithMetaMacro: reader [
        | form meta list |
        "pop reader macro token"
        reader next.
        meta := self readForm: reader.
        form := self readForm: reader.
        list := OrderedCollection from:
            { MALSymbol new: #'with-meta'. form. meta }.
        ^MALList new: list
    ]

    Reader class >> new: tokens [
        | reader |
        reader := super new.
        reader init: tokens.
        ^reader
    ]

    init: tokens [
        storage := tokens.
        index := 1.
    ]

    peek [
        ^storage at: index ifAbsent: [ nil ]
    ]

    next [
        | token |
        token := self peek.
        index := index + 1.
        ^token
    ]
]
