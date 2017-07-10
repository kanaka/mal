SequenceableCollection extend [
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

BlockClosure extend [
    valueWithExit [
        ^self value: [ ^nil ]
    ]
]

"NOTE: bugfix version from 3.2.91 for 3.2.4"
Namespace current: Kernel [

MatchingRegexResults extend [
    at: anIndex [
	<category: 'accessing'>
	| reg text |
	anIndex = 0 ifTrue: [^self match].
	cache isNil ifTrue: [cache := Array new: registers size].
	(cache at: anIndex) isNil
	    ifTrue:
		[reg := registers at: anIndex.
		text := reg isNil
			    ifTrue: [nil]
			    ifFalse: [
				reg isEmpty
					ifTrue: ['']
					ifFalse: [self subject copyFrom: reg first to: reg last]].
		cache at: anIndex put: text].
	^cache at: anIndex
    ]
]

]
