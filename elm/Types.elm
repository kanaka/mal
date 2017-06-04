module Types exposing (MalExpr(..), keywordPrefix)

import Array exposing (Array)
import Dict exposing (Dict)


type MalExpr
    = MalNil
    | MalBool Bool
    | MalInt Int
    | MalString String
    | MalKeyword String
    | MalSymbol String
    | MalList (List MalExpr)
    | MalVector (Array MalExpr)
    | MalMap (Dict String MalExpr)


{-| Keywords are prefixed by this char for usage in a MalMap.
Elm doesn't support user defined types as keys in a Dict.

The unicode char is: '\x029e'

-}
keywordPrefix : Char
keywordPrefix =
    'ʞ'
