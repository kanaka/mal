module Core exposing (..)

import Types exposing (MalExpr(..), MalFunction(..), Eval, Env)
import Env
import Eval
import Printer exposing (printString)
import Array
import IO exposing (IO(..))
import Reader


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

        {- = -}
        equals args =
            case args of
                [ a, b ] ->
                    Eval.succeed <| MalBool (a == b)

                _ ->
                    Eval.fail "unsupported arguments"

        {- pr-str -}
        prStr =
            List.map (printString True)
                >> String.join " "
                >> MalString
                >> Eval.succeed

        {- str -}
        str =
            List.map (printString False)
                >> String.join ""
                >> MalString
                >> Eval.succeed

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

        prn =
            List.map (printString True)
                >> String.join " "
                >> writeLine

        println =
            List.map (printString False)
                >> String.join " "
                >> writeLine

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

                UserFunc { fn } ->
                    fn args

        swap args =
            case args of
                (MalAtom atomId) :: (MalFunction func) :: args ->
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


malInit : List String
malInit =
    [ """(def! not
            (fn* (a)
                (if a false true)))"""
    ]
