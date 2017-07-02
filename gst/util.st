OrderedCollection extend [
    asDictionary [
        | dict assoc |
        dict := Dictionary new.
        1 to: self size by: 2 do:
            [ :i | dict add: (self at: i) -> (self at: i + 1) ].
        ^dict
    ]
]

String extend [
    parse [
        |text|
        text := self copyFrom: 2 to: self size - 1.
        text := text copyReplaceAll: '\"' with: '"'.
        text := text copyReplaceAll: '\n' with: '
'.
        text := text copyReplaceAll: '\\' with: '\'.
        ^text
    ]

    repr [
        |text|
        text := self copyReplaceAll: '\' with: '\\'.
        text := text copyReplaceAll: '
' with: '\n'.
        text := text copyReplaceAll: '"' with: '\"'.
        ^'"', text, '"'
    ]
]
