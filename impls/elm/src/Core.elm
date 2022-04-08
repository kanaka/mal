module Core exposing (..)

import Array
import Dict
import Env
import Eval
import IO exposing (IO(..))
import Printer exposing (printString)
import Reader
import Task
import Time
import Types exposing (..)
import Utils exposing (zip)


ns : Env
ns =
    let
        makeFn =
            CoreFunc Nothing >> MalFunction

        binaryOp fn retType args =
            case args of
                [ MalInt x, MalInt y ] ->
                    Eval.succeed (retType (fn x y))

                _ ->
                    Eval.fail "unsupported arguments"

        {- list -}
        core_list =
            Eval.succeed << MalList Nothing

        {- list? -}
        isList args =
            case args of
                [ MalList _ _ ] ->
                    Eval.succeed (MalBool True)

                _ ->
                    Eval.succeed (MalBool False)

        {- empty? -}
        isEmpty args =
            case args of
                [ MalList _ list ] ->
                    Eval.succeed <| MalBool (List.isEmpty list)

                [ MalVector _ vec ] ->
                    Eval.succeed <| MalBool (Array.isEmpty vec)

                _ ->
                    Eval.fail "unsupported arguments"

        {- count -}
        count args =
            case args of
                [ MalNil ] ->
                    Eval.succeed (MalInt 0)

                [ MalList _ list ] ->
                    Eval.succeed <| MalInt (List.length list)

                [ MalVector _ vec ] ->
                    Eval.succeed <| MalInt (Array.length vec)

                _ ->
                    Eval.fail "unsupported arguments"

        equalLists a b =
            case ( a, b ) of
                ( [], [] ) ->
                    True

                ( x :: xs, y :: ys ) ->
                    if deepEquals (x, y) then
                        equalLists xs ys

                    else
                        False

                _ ->
                    False

        compareListTo list other =
            case other of
                MalList _ otherList ->
                    equalLists list otherList

                MalVector _ vec ->
                    equalLists list (Array.toList vec)

                _ ->
                    False

        equalMaps a b =
            if Dict.keys a /= Dict.keys b then
                False

            else
                zip (Dict.values a) (Dict.values b)
                    |> List.map deepEquals
                    |> List.all identity

        deepEquals c =
            case c of
                ( MalList _ list, MalList _ otherList ) ->
                    equalLists list otherList

                ( MalList _ list, MalVector _ vec ) ->
                    equalLists list (Array.toList vec)

                ( MalList _ _, _ ) ->
                    False

                ( MalVector _ vec, MalList _ list ) ->
                    equalLists (Array.toList vec) list

                ( MalVector _ vec, MalVector _ otherVec ) ->
                    equalLists (Array.toList vec) (Array.toList otherVec)

                ( MalVector _ _, _ ) ->
                    False

                ( MalMap _ map, MalMap _ otherMap ) ->
                    equalMaps map otherMap

                ( MalMap _ _, _ ) ->
                    False

                ( _, MalMap _ _ ) ->
                    False

                (a, b) ->
                    a == b

        {- = -}
        equals args =
            case args of
                [ a, b ] ->
                    Eval.succeed <| MalBool (deepEquals (a, b))

                _ ->
                    Eval.fail "unsupported arguments"

        {- pr-str -}
        prStr args =
            Eval.withEnv
                (\env ->
                    args
                        |> List.map (printString env True)
                        |> String.join " "
                        |> MalString
                        |> Eval.succeed
                )

        {- str -}
        core_str args =
            Eval.withEnv
                (\env ->
                    args
                        |> List.map (printString env False)
                        |> String.join ""
                        |> MalString
                        |> Eval.succeed
                )

        {- helper function to write a string to stdout -}
        writeLine str =
            Eval.io (IO.writeLine str)
                (\msg ->
                    case msg of
                        LineWritten ->
                            Eval.succeed MalNil

                        _ ->
                            Eval.fail "wrong IO, expected LineWritten"
                )

        prn args =
            Eval.withEnv
                (\env ->
                    args
                        |> List.map (printString env True)
                        |> String.join " "
                        |> writeLine
                )

        println args =
            Eval.withEnv
                (\env ->
                    args
                        |> List.map (printString env False)
                        |> String.join " "
                        |> writeLine
                )

        printEnv args =
            case args of
                [] ->
                    Eval.withEnv (Printer.printEnv >> writeLine)

                _ ->
                    Eval.fail "unsupported arguments"

        readString args =
            case args of
                [ MalString str ] ->
                    case Reader.readString str of
                        Ok ast ->
                            Eval.succeed ast

                        Err msg ->
                            Eval.fail msg

                _ ->
                    Eval.fail "unsupported arguments"

        slurp args =
            case args of
                [ MalString filename ] ->
                    Eval.io (IO.readFile filename)
                        (\msg ->
                            case msg of
                                FileRead contents ->
                                    Eval.succeed <| MalString contents

                                Exception errMsg ->
                                    Eval.fail errMsg

                                _ ->
                                    Eval.fail "wrong IO, expected FileRead"
                        )

                _ ->
                    Eval.fail "unsupported arguments"

        atom args =
            case args of
                [ value ] ->
                    Eval.withEnv
                        (\env ->
                            case Env.newAtom value env of
                                ( newEnv, atomId ) ->
                                    Eval.setEnv newEnv
                                        |> Eval.map (\_ -> MalAtom atomId)
                        )

                _ ->
                    Eval.fail "unsupported arguments"

        isAtom args =
            case args of
                [ MalAtom _ ] ->
                    Eval.succeed <| MalBool True

                _ ->
                    Eval.succeed <| MalBool False

        deref args =
            case args of
                [ MalAtom atomId ] ->
                    Eval.withEnv (Env.getAtom atomId >> Eval.succeed)

                _ ->
                    Eval.fail "unsupported arguments"

        reset args =
            case args of
                [ MalAtom atomId, value ] ->
                    Eval.modifyEnv (Env.setAtom atomId value)
                        |> Eval.map (always value)

                _ ->
                    Eval.fail "unsupported arguments"

        {- helper function for calling a core or user function -}
        callFn func args =
            case func of
                CoreFunc _ fn ->
                    fn args

                UserFunc { eagerFn } ->
                    eagerFn args

        swap args =
            case args of
                (MalAtom atomId) :: (MalFunction func) :: moreArgs ->
                    Eval.withEnv
                        (\env ->
                            let
                                value =
                                    Env.getAtom atomId env
                            in
                            callFn func (value :: moreArgs)
                        )
                        |> Eval.andThen
                            (\res ->
                                Eval.modifyEnv (Env.setAtom atomId res)
                                    |> Eval.map (always res)
                            )

                _ ->
                    Eval.fail "unsupported arguments"

        gc args =
            Eval.withEnv (Env.gc MalNil >> Printer.printEnv >> writeLine)

        setDebug enabled =
            Eval.modifyEnv
                (\env ->
                    { env | debug = enabled }
                )
                |> Eval.andThen (\_ -> Eval.succeed MalNil)

        debug args =
            case args of
                [ MalBool value ] ->
                    setDebug value

                _ ->
                    Eval.withEnv
                        (\env ->
                            Eval.succeed (MalBool env.debug)
                        )

        typeof args =
            case args of
                [ MalInt _ ] ->
                    Eval.succeed <| MalSymbol "int"

                [ MalBool _ ] ->
                    Eval.succeed <| MalSymbol "bool"

                [ MalString _ ] ->
                    Eval.succeed <| MalSymbol "string"

                [ MalKeyword _ ] ->
                    Eval.succeed <| MalSymbol "keyword"

                [ MalSymbol _ ] ->
                    Eval.succeed <| MalSymbol "symbol"

                [ MalNil ] ->
                    Eval.succeed <| MalSymbol "nil"

                [ MalList _ _ ] ->
                    Eval.succeed <| MalSymbol "vector"

                [ MalVector _ _ ] ->
                    Eval.succeed <| MalSymbol "vector"

                [ MalMap _ _ ] ->
                    Eval.succeed <| MalSymbol "vector"

                [ MalFunction _ ] ->
                    Eval.succeed <| MalSymbol "function"

                [ MalAtom _ ] ->
                    Eval.succeed <| MalSymbol "atom"

                _ ->
                    Eval.fail "unsupported arguments"

        cons args =
            case args of
                [ e, MalList _ list ] ->
                    Eval.succeed <| MalList Nothing (e :: list)

                [ e, MalVector _ vec ] ->
                    Eval.succeed <| MalList Nothing (e :: (Array.toList vec))

                _ ->
                    Eval.fail "unsupported arguments"

        concat args =
            let
                go arg acc =
                    case arg of
                        MalList _ list ->
                            Eval.succeed (acc ++ list)

                        MalVector _ vec ->
                            Eval.succeed (acc ++ Array.toList vec)

                        _ ->
                            Eval.fail "unsupported arguments"
            in
            List.foldl (go >> Eval.andThen) (Eval.succeed []) args
                |> Eval.map (MalList Nothing)

        core_vec args =
            case args of
                [MalVector _ xs] -> Eval.succeed <| MalVector Nothing xs
                [MalList _   xs] -> Eval.succeed <| MalVector Nothing <| Array.fromList xs
                [_]            -> Eval.fail "vec: arg type"
                _              -> Eval.fail "vec: arg count"

        nth args =
            let
                get list index =
                    if index < 0 then
                        Nothing

                    else if index == 0 then
                        List.head list

                    else
                        case list of
                            [] ->
                                Nothing

                            _ :: rest ->
                                get rest (index - 1)

                make res =
                    case res of
                        Just value ->
                            Eval.succeed value

                        Nothing ->
                            Eval.fail "index out of bounds"
            in
            case args of
                [ MalList _ list, MalInt index ] ->
                    make <| get list index

                [ MalVector _ vec, MalInt index ] ->
                    make <| Array.get index vec

                _ ->
                    Eval.fail "unsupported arguments"

        first args =
            let
                make =
                    Eval.succeed << Maybe.withDefault MalNil
            in
            case args of
                [ MalNil ] ->
                    Eval.succeed MalNil

                [ MalList _ list ] ->
                    make <| List.head list

                [ MalVector _ vec ] ->
                    make <| Array.get 0 vec

                _ ->
                    Eval.fail "unsupported arguments"

        core_rest args =
            case args of
                [ MalNil ] ->
                    Eval.succeed <| MalList Nothing []

                [ MalList _ [] ] ->
                    Eval.succeed <| MalList Nothing []

                [ MalList _ (head :: tail) ] ->
                    Eval.succeed <| MalList Nothing tail

                [ MalVector _ vec ] ->
                    Array.toList vec
                        |> List.tail
                        |> Maybe.withDefault []
                        |> MalList Nothing
                        |> Eval.succeed

                _ ->
                    Eval.fail "unsupported arguments"

        throw args =
            case args of
                ex :: _ ->
                    Eval.throw ex

                _ ->
                    Eval.fail "undefined exception"

        apply args =
            case args of
                (MalFunction func) :: rest ->
                    case List.reverse rest of
                        (MalList _ last) :: middle ->
                            callFn func ((List.reverse middle) ++ last)

                        (MalVector _ last) :: middle ->
                            callFn func
                                ((List.reverse middle)
                                    ++ (Array.toList last)
                                )

                        _ ->
                            Eval.fail "apply expected the last argument to be a list or vector"

                _ ->
                    Eval.fail "unsupported arguments"

        core_map args =
            let
                go func list acc =
                    case list of
                        [] ->
                            Eval.succeed <| MalList Nothing <| List.reverse acc

                        inv :: rest ->
                            callFn func [ inv ]
                                |> Eval.andThen
                                    (\outv ->
                                        Eval.pushRef outv (go func rest (outv :: acc))
                                    )
            in
            case args of
                [ MalFunction func, MalList _ list ] ->
                    Eval.withStack (go func list [])

                [ MalFunction func, MalVector _ vec ] ->
                    go func (Array.toList vec) []

                _ ->
                    Eval.fail "unsupported arguments"

        isNil args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        MalNil :: _ ->
                            True

                        _ ->
                            False

        isTrue args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalBool True) :: _ ->
                            True

                        _ ->
                            False

        isFalse args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalBool False) :: _ ->
                            True

                        _ ->
                            False

        isNumber args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalInt _) :: _ ->
                            True

                        _ ->
                            False

        isSymbol args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalSymbol _) :: _ ->
                            True

                        _ ->
                            False

        isKeyword args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalKeyword _) :: _ ->
                            True

                        _ ->
                            False

        isVector args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalVector _ _) :: _ ->
                            True

                        _ ->
                            False

        isMap args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalMap _ _) :: _ ->
                            True

                        _ ->
                            False

        isString args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalString _) :: _ ->
                            True

                        _ ->
                            False

        isSequential args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalList _ _) :: _ ->
                            True

                        (MalVector _ _) :: _ ->
                            True

                        _ ->
                            False

        isFn args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalFunction (CoreFunc _ _)) :: _ ->
                            True
                        (MalFunction (UserFunc fn)) :: _ ->
                            not fn.isMacro

                        _ ->
                            False

        isMacro args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalFunction (UserFunc fn)) :: _ ->
                            fn.isMacro

                        _ ->
                            False

        symbol args =
            case args of
                [ MalString str ] ->
                    Eval.succeed <| MalSymbol str

                _ ->
                    Eval.fail "unsupported arguments"

        core_keyword args =
            case args of
                [ MalString str ] ->
                    Eval.succeed <| MalKeyword str

                [ (MalKeyword _) as kw ] ->
                    Eval.succeed kw

                _ ->
                    Eval.fail "unsupported arguments"

        vector args =
            Eval.succeed <| MalVector Nothing <| Array.fromList args

        parseKey key =
            case key of
                MalString str ->
                    Ok str

                MalKeyword keyword ->
                    Ok <| String.cons keywordPrefix keyword

                _ ->
                    Err "map key must be a symbol or keyword"

        buildMap list acc =
            case list of
                [] ->
                    Eval.succeed <| MalMap Nothing acc

                key :: value :: rest ->
                    parseKey key
                        |> Eval.fromResult
                        |> Eval.andThen
                            (\k ->
                                buildMap rest (Dict.insert k value acc)
                            )

                _ ->
                    Eval.fail "expected an even number of key-value pairs"

        hashMap args =
            buildMap args Dict.empty

        assoc args =
            case args of
                (MalMap _ dict) :: rest ->
                    buildMap rest dict

                _ ->
                    Eval.fail "unsupported arguments"

        dissoc args =
            let
                go keys acc =
                    case keys of
                        [] ->
                            Eval.succeed <| MalMap Nothing acc

                        key :: rest ->
                            parseKey key
                                |> Eval.fromResult
                                |> Eval.andThen
                                    (\k ->
                                        go rest (Dict.remove k acc)
                                    )
            in
            case args of
                (MalMap _ dict) :: keys ->
                    go keys dict

                _ ->
                    Eval.fail "unsupported arguments"

        core_get args =
            case args of
                [ MalNil, key ] ->
                    Eval.succeed MalNil

                [ MalMap _ dict, key ] ->
                    parseKey key
                        |> Eval.fromResult
                        |> Eval.map
                            (\k ->
                                Dict.get k dict
                                    |> Maybe.withDefault MalNil
                            )

                _ ->
                    Eval.fail "unsupported arguments"

        contains args =
            case args of
                [ MalMap _ dict, key ] ->
                    parseKey key
                        |> Eval.fromResult
                        |> Eval.map (\k -> Dict.member k dict)
                        |> Eval.map MalBool

                _ ->
                    Eval.fail "unsupported arguments"

        unparseKey key =
            case String.uncons key of
                Just ( prefix, rest ) ->
                    if prefix == keywordPrefix then
                        MalKeyword rest

                    else
                        MalString key

                _ ->
                    MalString key

        core_keys args =
            case args of
                [ MalMap _ dict ] ->
                    Dict.keys dict
                        |> List.map unparseKey
                        |> MalList Nothing
                        |> Eval.succeed

                _ ->
                    Eval.fail "unsupported arguments"

        vals args =
            case args of
                [ MalMap _ dict ] ->
                    Dict.values dict
                        |> MalList Nothing
                        |> Eval.succeed

                _ ->
                    Eval.fail "unsupported arguments"

        readLine args =
            case args of
                [ MalString prompt ] ->
                    Eval.io (IO.readLine prompt)
                        (\msg ->
                            case msg of
                                LineRead (Just line) ->
                                    Eval.succeed (MalString line)

                                LineRead Nothing ->
                                    Eval.succeed MalNil

                                _ ->
                                    Eval.fail "wrong IO, expected LineRead"
                        )

                _ ->
                    Eval.fail "unsupported arguments"

        withMeta args =
            case args of
                [ MalFunction (UserFunc func), meta ] ->
                    Eval.succeed <| MalFunction <| UserFunc { func | meta = Just meta }

                [ MalList _ xs, meta ] ->
                    Eval.succeed <| MalList (Just meta) xs

                [ MalVector _ xs, meta ] ->
                    Eval.succeed <| MalVector (Just meta) xs

                [ MalMap _ map, meta ] ->
                    Eval.succeed <| MalMap (Just meta) map

                [ MalFunction (CoreFunc _ f), meta ] ->
                    Eval.succeed <| MalFunction (CoreFunc (Just meta) f)

                _ ->
                    Eval.fail "with-meta expected a user function and a map"

        core_meta args =
            case args of
                [ MalFunction (UserFunc { meta }) ] ->
                    Eval.succeed (Maybe.withDefault MalNil meta)

                [ MalFunction (CoreFunc meta f) ] ->
                    Eval.succeed (Maybe.withDefault MalNil meta)

                [ MalList meta _ ] ->
                    Eval.succeed (Maybe.withDefault MalNil meta)

                [ MalVector meta _ ] ->
                    Eval.succeed (Maybe.withDefault MalNil meta)

                [ MalMap meta _ ] ->
                    Eval.succeed (Maybe.withDefault MalNil meta)

                _ ->
                    Eval.succeed MalNil

        conj args =
            case args of
                (MalList _ list) :: rest ->
                    Eval.succeed <|
                        MalList Nothing <|
                            List.reverse rest
                                ++ list

                (MalVector _ vec) :: rest ->
                    Eval.succeed <|
                        MalVector Nothing <|
                            Array.append
                                vec
                                (Array.fromList rest)

                _ ->
                    Eval.fail "unsupported arguments"

        seq args =
            case args of
                [ MalNil ] ->
                    Eval.succeed MalNil

                [ MalList _ [] ] ->
                    Eval.succeed MalNil

                [ MalString "" ] ->
                    Eval.succeed MalNil

                [ MalList _ xs ] ->
                    Eval.succeed (MalList Nothing xs)

                [ MalVector _ vec ] ->
                    Eval.succeed <|
                        if Array.isEmpty vec then
                            MalNil

                        else
                            MalList Nothing <| Array.toList vec

                [ MalString str ] ->
                    Eval.succeed <|
                        MalList Nothing <|
                            (String.toList str
                                |> List.map String.fromChar
                                |> List.map MalString
                            )

                _ ->
                    Eval.fail "unsupported arguments"

        requestTime =
            Task.perform (GotTime >> Ok >> Input) Time.now

        timeMs args =
            case args of
                [] ->
                    Eval.io requestTime
                        (\msg ->
                            case msg of
                                GotTime time ->
                                    Time.posixToMillis time
                                        |> MalInt
                                        |> Eval.succeed

                                _ ->
                                    Eval.fail "wrong IO, expected GotTime"
                        )

                _ ->
                    Eval.fail "time-ms takes no arguments"
    in
    Env.global
        |> Env.set "+" (makeFn <| binaryOp (+) MalInt)
        |> Env.set "-" (makeFn <| binaryOp (-) MalInt)
        |> Env.set "*" (makeFn <| binaryOp (*) MalInt)
        |> Env.set "/" (makeFn <| binaryOp (//) MalInt)
        |> Env.set "<" (makeFn <| binaryOp (<) MalBool)
        |> Env.set ">" (makeFn <| binaryOp (>) MalBool)
        |> Env.set "<=" (makeFn <| binaryOp (<=) MalBool)
        |> Env.set ">=" (makeFn <| binaryOp (>=) MalBool)
        |> Env.set "list" (makeFn core_list)
        |> Env.set "list?" (makeFn isList)
        |> Env.set "empty?" (makeFn isEmpty)
        |> Env.set "count" (makeFn count)
        |> Env.set "=" (makeFn equals)
        |> Env.set "pr-str" (makeFn prStr)
        |> Env.set "str" (makeFn core_str)
        |> Env.set "prn" (makeFn prn)
        |> Env.set "println" (makeFn println)
        |> Env.set "pr-env" (makeFn printEnv)
        |> Env.set "read-string" (makeFn readString)
        |> Env.set "slurp" (makeFn slurp)
        |> Env.set "atom" (makeFn atom)
        |> Env.set "atom?" (makeFn isAtom)
        |> Env.set "deref" (makeFn deref)
        |> Env.set "reset!" (makeFn reset)
        |> Env.set "swap!" (makeFn swap)
        |> Env.set "gc" (makeFn gc)
        |> Env.set "debug!" (makeFn debug)
        |> Env.set "typeof" (makeFn typeof)
        |> Env.set "cons" (makeFn cons)
        |> Env.set "concat" (makeFn concat)
        |> Env.set "vec" (makeFn core_vec)
        |> Env.set "nth" (makeFn nth)
        |> Env.set "first" (makeFn first)
        |> Env.set "rest" (makeFn core_rest)
        |> Env.set "throw" (makeFn throw)
        |> Env.set "apply" (makeFn apply)
        |> Env.set "map" (makeFn core_map)
        |> Env.set "nil?" (makeFn isNil)
        |> Env.set "true?" (makeFn isTrue)
        |> Env.set "false?" (makeFn isFalse)
        |> Env.set "number?" (makeFn isNumber)
        |> Env.set "symbol?" (makeFn isSymbol)
        |> Env.set "keyword?" (makeFn isKeyword)
        |> Env.set "vector?" (makeFn isVector)
        |> Env.set "map?" (makeFn isMap)
        |> Env.set "string?" (makeFn isString)
        |> Env.set "sequential?" (makeFn isSequential)
        |> Env.set "fn?" (makeFn isFn)
        |> Env.set "macro?" (makeFn isMacro)
        |> Env.set "symbol" (makeFn symbol)
        |> Env.set "keyword" (makeFn core_keyword)
        |> Env.set "vector" (makeFn vector)
        |> Env.set "hash-map" (makeFn hashMap)
        |> Env.set "assoc" (makeFn assoc)
        |> Env.set "dissoc" (makeFn dissoc)
        |> Env.set "get" (makeFn core_get)
        |> Env.set "contains?" (makeFn contains)
        |> Env.set "keys" (makeFn core_keys)
        |> Env.set "vals" (makeFn vals)
        |> Env.set "readline" (makeFn readLine)
        |> Env.set "with-meta" (makeFn withMeta)
        |> Env.set "meta" (makeFn core_meta)
        |> Env.set "conj" (makeFn conj)
        |> Env.set "seq" (makeFn seq)
        |> Env.set "time-ms" (makeFn timeMs)
