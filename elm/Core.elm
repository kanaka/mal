module Core exposing (..)

import Types exposing (MalExpr(..), MalFunction(..), Eval, Env)
import Env
import Eval
import Printer exposing (printString)
import Array
import Dict
import IO exposing (IO(..))
import Reader
import Utils exposing (zip)


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
                [ MalList list ] ->
                    Eval.succeed <| MalBool (List.isEmpty list)

                [ MalVector vec ] ->
                    Eval.succeed <| MalBool (Array.isEmpty vec)

                _ ->
                    Eval.fail "unsupported arguments"

        {- count -}
        count args =
            case args of
                [ MalNil ] ->
                    Eval.succeed (MalInt 0)

                [ MalList list ] ->
                    Eval.succeed <| MalInt (List.length list)

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

        compareListTo list other =
            case other of
                MalList otherList ->
                    equalLists list otherList

                MalVector vec ->
                    equalLists list (Array.toList vec)

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
                ( MalList list, MalList otherList ) ->
                    equalLists list otherList

                ( MalList list, MalVector vec ) ->
                    equalLists list (Array.toList vec)

                ( MalList _, _ ) ->
                    False

                ( MalVector vec, MalList list ) ->
                    equalLists (Array.toList vec) list

                ( MalVector vec, MalVector otherVec ) ->
                    equalLists (Array.toList vec) (Array.toList otherVec)

                ( MalVector _, _ ) ->
                    False

                ( MalMap map, MalMap otherMap ) ->
                    equalMaps map otherMap

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

                                Exception msg ->
                                    Eval.fail msg

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
                (MalAtom atomId) :: (MalFunction func) :: args ->
                    -- TODO eval apply here!
                    Eval.withEnv
                        (\env ->
                            let
                                value =
                                    Env.getAtom atomId env
                            in
                                callFn func (value :: args)
                        )
                        |> Eval.andThen
                            (\res ->
                                Eval.modifyEnv (Env.setAtom atomId res)
                                    |> Eval.map (always res)
                            )

                _ ->
                    Eval.fail "unsupported arguments"

        gc args =
            Eval.withEnv (Env.gc >> Printer.printEnv >> writeLine)

        setDebug enabled =
            Eval.modifyEnv
                (\env ->
                    { env | debug = enabled }
                )
                |> Eval.andThen (\_ -> Eval.succeed MalNil)

        debug args =
            case args of
                [ MalBool True ] ->
                    setDebug True

                _ ->
                    setDebug False

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


malInit : List String
malInit =
    [ """(def! not
            (fn* (a)
                (if a false true)))"""
    , """(def! load-file
            (fn* (f)
                (eval (read-string
                    (str "(do " (slurp f) ")")))))"""
    ]
