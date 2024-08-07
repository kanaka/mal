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

    MAL class >> starts_with: ast sym: sym  [
        | a a0 |
        ast type = #list ifFalse: [ ^false. ].
        a := ast value.
        a isEmpty ifTrue: [ ^false. ].
        a0 := a first.
        ^a0 type = #symbol and: [ a0 value = sym ].
    ]

    MAL class >> quasiquote: ast [
        | result acc |
        (ast type = #symbol or: [ ast type = #map ]) ifTrue: [
            result := {MALSymbol new: #quote. ast}.
            ^MALList new: (OrderedCollection from: result)
        ].
        (ast type = #list or: [ ast type = #vector ]) ifFalse: [
            ^ast
        ].

        (self starts_with: ast sym: #unquote) ifTrue: [
            ^ast value second
        ].

        result := {}.
        acc := MALList new: (OrderedCollection from: result).
        ast value reverseDo: [ : elt |
            (self starts_with: elt sym: #'splice-unquote') ifTrue: [
                result := {MALSymbol new: #concat. elt value second. acc}
            ] ifFalse: [
                result := {MALSymbol new: #cons. self quasiquote: elt. acc}
            ].
            acc := MALList new: (OrderedCollection from: result)
        ].
        ast type = #vector ifTrue: [
            result := {MALSymbol new: #vec. acc}.
            acc := MALList new: (OrderedCollection from: result)
        ].
        ^acc
    ]

    MAL class >> EVAL: aSexp env: anEnv [
        | sexp env ast a0 a0_ a1 a1_ a2 a3 function args |

        "NOTE: redefinition of method arguments is not allowed"
        sexp := aSexp.
        env := anEnv.

        [
            [ :continue |

                a0 := env get: #'DEBUG-EVAL'.
                (a0 isNil or: [ a0 type = #false or: [ a0 type = #nil ] ] )
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
                a0 := ast first.

                a0_ := ast first value.
                a0_ = #'def!' ifTrue: [
                    | result |
                    a1_ := ast second value.
                    a2 := ast third.
                    result := self EVAL: a2 env: env.
                    env set: a1_ value: result.
                    ^result
                ].

                a0_ = #'defmacro!' ifTrue: [
                    | result |
                    a1_ := ast second value.
                    a2 := ast third.
                    result := (self EVAL: a2 env: env) deepCopy.
                    result isMacro: true.
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

                a0_ = #quote ifTrue: [
                    a1 := ast second.
                    ^a1
                ].

                a0_ = #quasiquote ifTrue: [
                    | result |
                    a1 := ast second.
                    sexp := self quasiquote: a1.
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

                function := self EVAL: a0 env: env.
                args := ast allButFirst asArray.
                (function type = #func and: [ function isMacro ]) ifTrue: [
                    sexp := function fn value: args.
                    continue value TCO
                ].
                args := self evalList: args env: env.
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

| input historyFile replEnv argv |

historyFile := '.mal_history'.
ReadLine readHistory: historyFile.
replEnv := Env new: nil.

argv := Smalltalk arguments.
argv notEmpty ifTrue: [ argv := argv allButFirst ].
argv := OrderedCollection from: (argv collect: [ :arg | MALString new: arg ]).

Core Ns keysAndValuesDo: [ :op :block | replEnv set: op value: block ].
replEnv set: #eval value: (Fn new: [ :args | MAL EVAL: args first env: replEnv ]).
replEnv set: #'*ARGV*' value: (MALList new: argv).

MAL rep: '(def! not (fn* (a) (if a false true)))' env: replEnv.
MAL rep: '(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))' env: replEnv.
MAL rep: '(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))' env: replEnv.

Smalltalk arguments notEmpty ifTrue: [
    MAL rep: '(load-file "', Smalltalk arguments first, '")' env: replEnv
] ifFalse: [
    [ input := ReadLine readLine: 'user> '. input isNil ] whileFalse: [
        input isEmpty ifFalse: [
            ReadLine addHistory: input.
            ReadLine writeHistory: historyFile.
            [ (MAL rep: input env: replEnv) displayNl ]
                on: MALEmptyInput do: [ #return ]
                on: MALError do:
                    [ :err | ('error: ', err messageText) displayNl. #return ].
        ]
    ].

    '' displayNl.
]
