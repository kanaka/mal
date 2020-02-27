module Printer exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Types exposing (Env, MalExpr(..), keywordPrefix, MalFunction(..))
import Utils exposing (encodeString, wrap)
import Env


printStr : Bool -> MalExpr -> String
printStr =
    printString Env.global


printString : Env -> Bool -> MalExpr -> String
printString env readably ast =
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
            printRawString env readably str

        MalSymbol sym ->
            sym

        MalKeyword kw ->
            kw

        MalList list ->
            printList env readably list

        MalVector vec ->
            printVector env readably vec

        MalMap map ->
            printMap env readably map

        MalFunction _ ->
            "#<function>"

        MalAtom atomId ->
            let
                value =
                    Env.getAtom atomId env
            in
                "(atom " ++ (printString env True value) ++ ")"

        MalApply _ ->
            "#<apply>"


printBound : Env -> Bool -> List ( String, MalExpr ) -> String
printBound env readably =
    let
        printEntry name value =
            name ++ "=" ++ (printString env readably value)
    in
        List.map (uncurry printEntry)
            >> String.join " "
            >> wrap "(" ")"


printRawString : Env -> Bool -> String -> String
printRawString env readably str =
    if readably then
        encodeString str
    else
        str


printList : Env -> Bool -> List MalExpr -> String
printList env readably =
    List.map (printString env readably)
        >> String.join " "
        >> wrap "(" ")"


printVector : Env -> Bool -> Array MalExpr -> String
printVector env readably =
    Array.map (printString env readably)
        >> Array.toList
        >> String.join " "
        >> wrap "[" "]"


printMap : Env -> Bool -> Dict String MalExpr -> String
printMap env readably =
    let
        -- Strip off the keyword prefix if it is there.
        printKey k =
            case String.uncons k of
                Just ( prefix, rest ) ->
                    if prefix == keywordPrefix then
                        rest
                    else
                        printRawString env readably k

                _ ->
                    printRawString env readably k

        printEntry ( k, v ) =
            (printKey k) ++ " " ++ (printString env readably v)
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

        printHeader frameId { outerId, exitId, refCnt } =
            "#"
                ++ (toString frameId)
                ++ " outer="
                ++ printOuterId outerId
                ++ " exit="
                ++ printOuterId exitId
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
            (k ++ " = " ++ (printString env False v)) :: acc
    in
        "--- Environment ---\n"
            ++ "Current frame: #"
            ++ (toString env.currentFrameId)
            ++ "\n\n"
            ++ String.join "\n\n" (Dict.foldr printFrameAcc [] env.frames)
