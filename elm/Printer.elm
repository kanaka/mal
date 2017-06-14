module Printer exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Types exposing (Env, MalExpr(..), keywordPrefix)
import Utils exposing (encodeString, wrap)


printString : Bool -> MalExpr -> String
printString readably ast =
    case ast of
        MalNil ->
            "nil"

        MalBool True ->
            "true"

        MalBool False ->
            "false"

        MalInt int ->
            toString int

        MalString str ->
            printRawString readably str

        MalSymbol sym ->
            sym

        MalKeyword kw ->
            kw

        MalList list ->
            printList readably list

        MalVector vec ->
            printVector readably vec

        MalMap map ->
            printMap readably map

        MalFunction _ ->
            "#<function>"

        MalAtom atomId ->
            "#<atom:" ++ (toString atomId) ++ ">"

        MalApply _ ->
            "#<apply>"


printRawString : Bool -> String -> String
printRawString readably str =
    if readably then
        encodeString str
    else
        str


printList : Bool -> List MalExpr -> String
printList readably =
    List.map (printString readably)
        >> String.join " "
        >> wrap "(" ")"


printVector : Bool -> Array MalExpr -> String
printVector readably =
    Array.map (printString readably)
        >> Array.toList
        >> String.join " "
        >> wrap "[" "]"


printMap : Bool -> Dict String MalExpr -> String
printMap readably =
    let
        -- Strip off the keyword prefix if it is there.
        printKey k =
            case String.uncons k of
                Just ( prefix, rest ) ->
                    if prefix == keywordPrefix then
                        rest
                    else
                        printRawString readably k

                _ ->
                    printRawString readably k

        printEntry ( k, v ) =
            (printKey k) ++ " " ++ (printString readably v)
    in
        Dict.toList
            >> List.map printEntry
            >> String.join " "
            >> wrap "{" "}"


printEnv : Env -> String
printEnv env =
    let
        printOuterId =
            Maybe.map toString >> Maybe.withDefault "nil"

        printHeader frameId { outerId, refCnt } =
            "#"
                ++ (toString frameId)
                ++ " outer="
                ++ printOuterId outerId
                ++ " refCnt="
                ++ (toString refCnt)

        printFrame frameId frame =
            String.join "\n"
                ((printHeader frameId frame)
                    :: (Dict.foldr printDatum [] frame.data)
                )

        printFrameAcc k v acc =
            printFrame k v :: acc

        printDatum k v acc =
            (k ++ " = " ++ (printString True v)) :: acc
    in
        "--- Environment ---\n"
            ++ "Current frame: #"
            ++ (toString env.currentFrameId)
            ++ "\n\n"
            ++ String.join "\n\n" (Dict.foldr printFrameAcc [] env.frames)
