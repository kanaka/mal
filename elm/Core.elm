module Core exposing (..)

import Types exposing (MalExpr(..), Eval, Env)
import Env
import Eval
import Printer exposing (printString)
import Array
import IO exposing (IO(..))


ns : Env
ns =
    let
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

        writeLine str =
            Eval.io (IO.writeLine str)
                (\msg ->
                    case msg of
                        LineWritten ->
                            -- TODO need caller continuation here...
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
    in
        Env.make Nothing
            |> Env.set "+" (MalFunction <| binaryOp (+) MalInt)
            |> Env.set "-" (MalFunction <| binaryOp (-) MalInt)
            |> Env.set "*" (MalFunction <| binaryOp (*) MalInt)
            |> Env.set "/" (MalFunction <| binaryOp (//) MalInt)
            |> Env.set "<" (MalFunction <| binaryOp (<) MalBool)
            |> Env.set ">" (MalFunction <| binaryOp (>) MalBool)
            |> Env.set "<=" (MalFunction <| binaryOp (<=) MalBool)
            |> Env.set ">=" (MalFunction <| binaryOp (>=) MalBool)
            |> Env.set "list" (MalFunction list)
            |> Env.set "list?" (MalFunction isList)
            |> Env.set "empty?" (MalFunction isEmpty)
            |> Env.set "count" (MalFunction count)
            |> Env.set "=" (MalFunction equals)
            |> Env.set "pr-str" (MalFunction prStr)
            |> Env.set "str" (MalFunction str)
            |> Env.set "prn" (MalFunction prn)
            |> Env.set "println" (MalFunction println)
