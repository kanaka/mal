Object subclass: MALObject [
    | type value meta |

    type [ ^type ]
    value [ ^value ]
    meta [ ^meta ]

    value: aValue [
        value := aValue.
    ]

    meta: aMeta [
        meta := aMeta.
    ]

    MALObject class >> new: type value: value meta: meta [
        | object |
        object := super new.
        object init: type value: value meta: meta.
        ^object
    ]

    init: aType value: aValue meta: aMeta [
        type := aType.
        value := aValue.
        meta := aMeta.
    ]

    withMeta: meta [
        | object |
        object := self deepCopy.
        object meta: meta.
        ^object
    ]

    printOn: stream [
        stream nextPutAll: '<';
            nextPutAll: self class printString;
            nextPutAll: ': ';
            nextPutAll: value printString.
        meta notNil ifTrue: [
            stream nextPutAll: ' | '
                nextPutAll: meta printString.
        ].
        stream nextPutAll: '>'.
    ]

    = x [
        self type ~= x type ifTrue: [ ^false ].
        ^self value = x value
    ]

    hash [
        ^self value hash
    ]
]

MALObject subclass: MALTrue [
    MALTrue class >> new [
        ^super new: #true value: true meta: nil.
    ]
]

MALObject subclass: MALFalse [
    MALFalse class >> new [
        ^super new: #false value: false meta: nil.
    ]
]

MALObject subclass: MALNil [
    MALNil class >> new [
        ^super new: #nil value: nil meta: nil.
    ]
]

MALObject class extend [
    True := MALTrue new.
    False := MALFalse new.
    Nil := MALNil new.

    True [ ^True ]
    False [ ^False ]
    Nil [ ^Nil ]
]

MALObject subclass: MALNumber [
    MALNumber class >> new: value [
        ^super new: #number value: value meta: nil.
    ]
]

MALObject subclass: MALString [
    MALString class >> new: value [
        ^super new: #string value: value meta: nil.
    ]
]

MALObject subclass: MALSymbol [
    MALSymbol class >> new: value [
        ^super new: #symbol value: value meta: nil.
    ]
]

MALObject subclass: MALKeyword [
    MALKeyword class >> new: value [
        ^super new: #keyword value: value meta: nil.
    ]
]

MALObject subclass: MALList [
    MALList class >> new: value [
        ^super new: #list value: value meta: nil.
    ]

    = x [
        (x type ~= #list and: [ x type ~= #vector ]) ifTrue: [ ^false ].
        ^self value = x value
    ]
]

MALObject subclass: MALVector [
    MALVector class >> new: value [
        ^super new: #vector value: value meta: nil.
    ]

    = x [
        (x type ~= #vector and: [ x type ~= #list ]) ifTrue: [ ^false ].
        ^self value = x value
    ]
]

MALObject subclass: MALMap [
    MALMap class >> new: value [
        ^super new: #map value: value meta: nil.
    ]
]

MALObject subclass: MALAtom [
    MALAtom class >> new: value [
        ^super new: #atom value: value meta: nil.
    ]
]

MALObject subclass: Fn [
    | fn |

    fn [ ^fn ]

    Fn class >> new: fn [
        | f |
        f := super new: #fn value: fn meta: nil.
        f init: fn.
        ^f
    ]

    init: f [
        fn := f.
    ]
]

Error subclass: MALError [
    description [ ^'A MAL-related error' ]
    isResumable [ ^true ]

    data [ ^self messageText ]
]

MALError subclass: MALUnterminatedSequence [
    MALUnterminatedSequence class >> new [ ^super new ]

    messageText [ ^'expected ''', self basicMessageText, ''', got EOF' ]
]

MALError subclass: MALUnexpectedToken [
    MALUnexpectedToken class >> new [ ^super new ]

    messageText [ ^'unexpected token: ''', self basicMessageText, '''']
]

MALError subclass: MALEmptyInput [
    MALEmptyInput class >> new [ ^super new ]

    messageText [ ^'Empty input' ]
]

MALError subclass: MALUnknownSymbol [
    MALUnknownSymbol class >> new [ ^super new ]

    messageText [ ^'''', self basicMessageText, ''' not found']
]

MALError subclass: MALOutOfBounds [
    MALOutOfBounds class >> new [ ^super new ]

    messageText [ ^'Out of bounds' ]
]

MALError subclass: MALCustomError [
    MALCustomError class >> new [ ^super new ]

    messageText [ ^Printer prStr: self basicMessageText printReadably: true ]
    data [ ^self basicMessageText ]
]
