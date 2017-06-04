module Printer exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Types exposing (MalExpr(..), keywordPrefix)
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
