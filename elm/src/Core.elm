module Core exposing (..)

import Types exposing (..)
import Env
import Eval
import Printer exposing (printString)
import Array
import Dict
import IO exposing (IO(..))
import Reader
import Utils exposing (zip)
import Time
import Task


ns : Env
ns =
    let
        makeFn =
            CoreFunc >> MalFunction

        binaryOp fn retType args =
            case args of
                [ MalInt x, MalInt y ] ->
                    Eval.succeed (retType (fn x y))

                _ ->
                    Eval.fail "unsupported arguments"

        {- list -}
        list =
            Eval.succeed << MalList

        {- list? -}
        isList args =
            case args of
                [ MalList _ ] ->
                    Eval.succeed (MalBool True)

                _ ->
                    Eval.succeed (MalBool False)

        {- empty? -}
        isEmpty args =
            case args of
                [ MalList list_ ] ->
                    Eval.succeed <| MalBool (List.isEmpty list_)

                [ MalVector vec ] ->
                    Eval.succeed <| MalBool (Array.isEmpty vec)

                _ ->
                    Eval.fail "unsupported arguments"

        {- count -}
        count args =
            case args of
                [ MalNil ] ->
                    Eval.succeed (MalInt 0)

                [ MalList list_ ] ->
                    Eval.succeed <| MalInt (List.length list_)

                [ MalVector vec ] ->
                    Eval.succeed <| MalInt (Array.length vec)

                _ ->
                    Eval.fail "unsupported arguments"

        equalLists a b =
            case ( a, b ) of
                ( [], [] ) ->
                    True

                ( x :: xs, y :: ys ) ->
                    if deepEquals x y then
                        equalLists xs ys
                    else
                        False

                _ ->
                    False

        compareListTo list_ other =
            case other of
                MalList otherList ->
                    equalLists list_ otherList

                MalVector vec ->
                    equalLists list_ (Array.toList vec)

                _ ->
                    False

        equalMaps a b =
            if Dict.keys a /= Dict.keys b then
                False
            else
                zip (Dict.values a) (Dict.values b)
                    |> List.map (uncurry deepEquals)
                    |> List.all identity

        deepEquals a b =
            case ( a, b ) of
                ( MalList list_, MalList otherList ) ->
                    equalLists list_ otherList

                ( MalList list_, MalVector vec ) ->
                    equalLists list_ (Array.toList vec)

                ( MalList _, _ ) ->
                    False

                ( MalVector vec, MalList list_ ) ->
                    equalLists (Array.toList vec) list_

                ( MalVector vec, MalVector otherVec ) ->
                    equalLists (Array.toList vec) (Array.toList otherVec)

                ( MalVector _, _ ) ->
                    False

                ( MalMap map_, MalMap otherMap ) ->
                    equalMaps map_ otherMap

                ( MalMap _, _ ) ->
                    False

                ( _, MalMap _ ) ->
                    False

                _ ->
                    a == b

        {- = -}
        equals args =
            case args of
                [ a, b ] ->
                    Eval.succeed <| MalBool (deepEquals a b)

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
        str args =
            Eval.withEnv
                (\env ->
                    args
                        |> List.map (printString env False)
                        |> String.join ""
                        |> MalString
                        |> Eval.succeed
                )

        {- helper function to write a string to stdout -}
        writeLine str_ =
            Eval.io (IO.writeLine str_)
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
                [ MalString str_ ] ->
                    case Reader.readString str_ of
                        Ok Nothing ->
                            Eval.succeed MalNil

                        Ok (Just ast) ->
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

                                Exception msg_ ->
                                    Eval.fail msg_

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
                CoreFunc fn ->
                    fn args

                UserFunc { eagerFn } ->
                    eagerFn args

        swap args =
            case args of
                (MalAtom atomId) :: (MalFunction func) :: args_ ->
                    Eval.withEnv
                        (\env ->
                            let
                                value =
                                    Env.getAtom atomId env
                            in
                                callFn func (value :: args_)
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

                [ MalList _ ] ->
                    Eval.succeed <| MalSymbol "vector"

                [ MalVector _ ] ->
                    Eval.succeed <| MalSymbol "vector"

                [ MalMap _ ] ->
                    Eval.succeed <| MalSymbol "vector"

                [ MalFunction _ ] ->
                    Eval.succeed <| MalSymbol "function"

                [ MalAtom _ ] ->
                    Eval.succeed <| MalSymbol "atom"

                _ ->
                    Eval.fail "unsupported arguments"

        cons args =
            case args of
                [ e, MalList list_ ] ->
                    Eval.succeed <| MalList (e :: list_)

                [ e, MalVector vec ] ->
                    Eval.succeed <| MalList (e :: (Array.toList vec))

                _ ->
                    Eval.fail "unsupported arguments"

        concat args =
            let
                go arg acc =
                    case arg of
                        MalList list_ ->
                            Eval.succeed (acc ++ list_)

                        MalVector vec ->
                            Eval.succeed (acc ++ Array.toList vec)

                        _ ->
                            Eval.fail "unsupported arguments"
            in
                List.foldl (go >> Eval.andThen) (Eval.succeed []) args
                    |> Eval.map MalList

        nth args =
            let
                getFunc list_ index =
                    if index < 0 then
                        Nothing
                    else if index == 0 then
                        List.head list_
                    else
                        case list_ of
                            [] ->
                                Nothing

                            _ :: rest_ ->
                                getFunc rest_ (index - 1)

                make res =
                    case res of
                        Just value ->
                            Eval.succeed value

                        Nothing ->
                            Eval.fail "index out of bounds"
            in
                case args of
                    [ MalList list_, MalInt index ] ->
                        make <| getFunc list_ index

                    [ MalVector vec, MalInt index ] ->
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

                    [ MalList list_ ] ->
                        make <| List.head list_

                    [ MalVector vec ] ->
                        make <| Array.get 0 vec

                    _ ->
                        Eval.fail "unsupported arguments"

        rest args =
            case args of
                [ MalNil ] ->
                    Eval.succeed <| MalList []

                [ MalList [] ] ->
                    Eval.succeed <| MalList []

                [ MalList (head :: tail) ] ->
                    Eval.succeed <| MalList tail

                [ MalVector vec ] ->
                    Array.toList vec
                        |> List.tail
                        |> Maybe.withDefault []
                        |> MalList
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
                (MalFunction func) :: rest_ ->
                    case List.reverse rest_ of
                        (MalList last) :: middle ->
                            callFn func ((List.reverse middle) ++ last)

                        (MalVector last) :: middle ->
                            callFn func
                                ((List.reverse middle)
                                    ++ (Array.toList last)
                                )

                        _ ->
                            Eval.fail "apply expected the last argument to be a list or vector"

                _ ->
                    Eval.fail "unsupported arguments"

        map args =
            let
                go func list_ acc =
                    case list_ of
                        [] ->
                            Eval.succeed <| MalList <| List.reverse acc

                        inv :: rest_ ->
                            callFn func [ inv ]
                                |> Eval.andThen
                                    (\outv ->
                                        Eval.pushRef outv (go func rest_ (outv :: acc))
                                    )
            in
                case args of
                    [ MalFunction func, MalList list_ ] ->
                        Eval.withStack (go func list_ [])

                    [ MalFunction func, MalVector vec ] ->
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
                        (MalVector _) :: _ ->
                            True

                        _ ->
                            False

        isMap args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalMap _) :: _ ->
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
                        (MalList _) :: _ ->
                            True

                        (MalVector _) :: _ ->
                            True

                        _ ->
                            False

        isFn args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalFunction (CoreFunc _)) :: _ ->
                            True
                        (MalFunction (UserFunc fn)) :: _ ->
                            if fn.isMacro then
                                False
                            else
                                True

                        _ ->
                            False

        isMacro args =
            Eval.succeed <|
                MalBool <|
                    case args of
                        (MalFunction (UserFunc fn)) :: _ ->
                            if fn.isMacro then
                                True
                            else
                                False

                        _ ->
                            False

        symbol args =
            case args of
                [ MalString str_ ] ->
                    Eval.succeed <| MalSymbol str_

                _ ->
                    Eval.fail "unsupported arguments"

        keyword args =
            case args of
                [ MalString str_ ] ->
                    Eval.succeed <| MalKeyword (String.cons ':' str_)

                _ ->
                    Eval.fail "unsupported arguments"

        vector args =
            Eval.succeed <| MalVector <| Array.fromList args

        parseKey key =
            case key of
                MalString str_ ->
                    Ok str_

                MalKeyword keyword_ ->
                    Ok <| String.cons keywordPrefix keyword_

                _ ->
                    Err "map key must be a symbol or keyword"

        buildMap list_ acc =
            case list_ of
                [] ->
                    Eval.succeed <| MalMap acc

                key :: value :: rest_ ->
                    parseKey key
                        |> Eval.fromResult
                        |> Eval.andThen
                            (\key_ ->
                                buildMap rest_ (Dict.insert key_ value acc)
                            )

                _ ->
                    Eval.fail "expected an even number of key-value pairs"

        hashMap args =
            buildMap args Dict.empty

        assoc args =
            case args of
                (MalMap dict) :: rest_ ->
                    buildMap rest_ dict

                _ ->
                    Eval.fail "unsupported arguments"

        dissoc args =
            let
                go keys_ acc =
                    case keys_ of
                        [] ->
                            Eval.succeed <| MalMap acc

                        key :: rest_ ->
                            parseKey key
                                |> Eval.fromResult
                                |> Eval.andThen
                                    (\key_ ->
                                        go rest_ (Dict.remove key_ acc)
                                    )
            in
                case args of
                    (MalMap dict) :: keys_ ->
                        go keys_ dict

                    _ ->
                        Eval.fail "unsupported arguments"

        get args =
            case args of
                [ MalNil, key ] ->
                    Eval.succeed MalNil

                [ MalMap dict, key ] ->
                    parseKey key
                        |> Eval.fromResult
                        |> Eval.map
                            (\key_ ->
                                Dict.get key_ dict
                                    |> Maybe.withDefault MalNil
                            )

                _ ->
                    Eval.fail "unsupported arguments"

        contains args =
            case args of
                [ MalMap dict, key ] ->
                    parseKey key
                        |> Eval.fromResult
                        |> Eval.map (\key_ -> Dict.member key_ dict)
                        |> Eval.map MalBool

                _ ->
                    Eval.fail "unsupported arguments"

        unparseKey key =
            case String.uncons key of
                Just ( prefix, rest_ ) ->
                    if prefix == keywordPrefix then
                        MalKeyword rest_
                    else
                        MalString key

                _ ->
                    MalString key

        keys args =
            case args of
                [ MalMap dict ] ->
                    Dict.keys dict
                        |> List.map unparseKey
                        |> MalList
                        |> Eval.succeed

                _ ->
                    Eval.fail "unsupported arguments"

        vals args =
            case args of
                [ MalMap dict ] ->
                    Dict.values dict
                        |> MalList
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

                _ ->
                    Eval.fail "with-meta expected a user function and a map"

        meta_ args =
            case args of
                [ MalFunction (UserFunc { meta }) ] ->
                    Eval.succeed (Maybe.withDefault MalNil meta)

                _ ->
                    Eval.succeed MalNil

        conj args =
            case args of
                (MalList list_) :: rest_ ->
                    Eval.succeed <|
                        MalList <|
                            (List.reverse rest_)
                                ++ list_

                (MalVector vec) :: rest_ ->
                    Eval.succeed <|
                        MalVector <|
                            Array.append
                                vec
                                (Array.fromList rest_)

                _ ->
                    Eval.fail "unsupported arguments"

        seq args =
            case args of
                [ MalNil ] ->
                    Eval.succeed MalNil

                [ MalList [] ] ->
                    Eval.succeed MalNil

                [ MalString "" ] ->
                    Eval.succeed MalNil

                [ (MalList _) as list_ ] ->
                    Eval.succeed list_

                [ MalVector vec ] ->
                    Eval.succeed <|
                        if Array.isEmpty vec then
                            MalNil
                        else
                            MalList <| Array.toList vec

                [ MalString str_ ] ->
                    Eval.succeed <|
                        MalList <|
                            (String.toList str_
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
                                    Time.toMillis Time.utc time
                                        |> toFloat
                                        |> floor
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
            |> Env.set "list" (makeFn list)
            |> Env.set "list?" (makeFn isList)
            |> Env.set "empty?" (makeFn isEmpty)
            |> Env.set "count" (makeFn count)
            |> Env.set "=" (makeFn equals)
            |> Env.set "pr-str" (makeFn prStr)
            |> Env.set "str" (makeFn str)
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
            |> Env.set "nth" (makeFn nth)
            |> Env.set "first" (makeFn first)
            |> Env.set "rest" (makeFn rest)
            |> Env.set "throw" (makeFn throw)
            |> Env.set "apply" (makeFn apply)
            |> Env.set "map" (makeFn map)
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
            |> Env.set "keyword" (makeFn keyword)
            |> Env.set "vector" (makeFn vector)
            |> Env.set "hash-map" (makeFn hashMap)
            |> Env.set "assoc" (makeFn assoc)
            |> Env.set "dissoc" (makeFn dissoc)
            |> Env.set "get" (makeFn get)
            |> Env.set "contains?" (makeFn contains)
            |> Env.set "keys" (makeFn keys)
            |> Env.set "vals" (makeFn vals)
            |> Env.set "readline" (makeFn readLine)
            |> Env.set "with-meta" (makeFn withMeta)
            |> Env.set "meta" (makeFn meta_)
            |> Env.set "conj" (makeFn conj)
            |> Env.set "seq" (makeFn seq)
            |> Env.set "time-ms" (makeFn timeMs)


uncurry : (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) =
  f a b