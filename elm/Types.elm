module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import IO exposing (IO)


type Msg
    = Input (Result String IO)


type alias Frame =
    { outerId : Maybe Int
    , data : Dict String MalExpr
    , refCnt : Int
    }


type alias Env =
    { frames : Dict Int Frame
    , nextFrameId : Int
    , currentFrameId : Int
    , atoms : Dict Int MalExpr
    , nextAtomId : Int
    }


type EvalResult res
    = EvalErr String
    | EvalOk res
    | EvalIO (Cmd Msg) (IO -> Eval res)



-- TODO EvalTCO Env -> Eval MalExpr (?)


type alias EvalContext res =
    ( Env, EvalResult res )


type alias EvalFn res =
    Env -> EvalContext res


type Eval res
    = Eval (EvalFn res)


type alias MalFn =
    List MalExpr -> Eval MalExpr


type MalFunction
    = CoreFunc MalFn
    | UserFunc { frameId : Int, fn : MalFn }


type alias TcoFn =
    () -> Eval MalExpr


type alias Bound =
    List ( String, MalExpr )


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
    | MalFunction MalFunction
    | MalApply { frameId : Int, bound : Bound, body : MalExpr }
    | MalAtom Int


{-| Keywords are prefixed by this char for usage in a MalMap.
Elm doesn't support user defined types as keys in a Dict.

The unicode char is: '\x029e'

-}
keywordPrefix : Char
keywordPrefix =
    'ʞ'
