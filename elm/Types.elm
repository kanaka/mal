module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import IO exposing (IO)


type Either a b
    = Left a
    | Right b


type Msg
    = Input (Result String IO)


type alias Frame =
    { outerId : Maybe Int
    , exitId : Maybe Int
    , data : Dict String MalExpr
    , refCnt : Int
    }


type alias Env =
    { frames : Dict Int Frame
    , nextFrameId : Int
    , currentFrameId : Int
    , atoms : Dict Int MalExpr
    , nextAtomId : Int
    , debug : Bool
    , gcInterval : Int
    , gcCounter : Int
    , stack : List MalExpr
    , keepFrames : List Int
    }


type alias EvalCont a =
    IO -> Eval a


type EvalResult res
    = EvalErr MalExpr
    | EvalOk res
    | EvalIO (Cmd Msg) (EvalCont res)


type alias EvalContext res =
    ( Env, EvalResult res )


type alias Eval res =
    Env -> EvalContext res


type alias MalFn =
    List MalExpr -> Eval MalExpr


type MalFunction
    = CoreFunc MalFn
    | UserFunc
        { frameId : Int
        , lazyFn : MalFn
        , eagerFn : MalFn
        , isMacro : Bool
        , meta : Maybe MalExpr
        }


type alias ApplyRec =
    { frameId : Int, bound : Bound, body : MalExpr }


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
    | MalApply ApplyRec
    | MalAtom Int


{-| Keywords are prefixed by this char for usage in a MalMap.
Elm doesn't support user defined types as keys in a Dict.

The unicode char is: '\x029e'

-}
keywordPrefix : Char
keywordPrefix =
    'ʞ'
