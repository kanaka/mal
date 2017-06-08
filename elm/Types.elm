module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import IO exposing (IO)


type Msg
    = Input (Result String IO)


type Env
    = Env
        { outer : Maybe Env
        , data : Dict String MalExpr
        }


type alias EvalState =
    { env : Env
    }


type EvalResult res
    = EvalErr String
    | EvalOk res
    | EvalIO (Cmd Msg) (IO -> Eval res)


type alias EvalContext res =
    ( EvalState, EvalResult res )


type alias EvalFn res =
    EvalState -> EvalContext res


type Eval res
    = Eval (EvalFn res)


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
    | MalFunction (List MalExpr -> Eval MalExpr)


{-| Keywords are prefixed by this char for usage in a MalMap.
Elm doesn't support user defined types as keys in a Dict.

The unicode char is: '\x029e'

-}
keywordPrefix : Char
keywordPrefix =
    'ʞ'
