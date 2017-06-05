module Env exposing (Env, make, set, get)

import Types exposing (MalExpr(..))
import Dict exposing (Dict)


type Env
    = Env
        { outer : Maybe Env
        , data : Dict String MalExpr
        }


make : Maybe Env -> Env
make outer =
    Env { outer = outer, data = Dict.empty }


set : String -> MalExpr -> Env -> Env
set name expr (Env env) =
    Env { env | data = Dict.insert name expr env.data }


get : String -> Env -> Result String MalExpr
get name ((Env { outer, data }) as env) =
    case Dict.get name data of
        Just val ->
            Ok val

        Nothing ->
            outer
                |> Maybe.map (get name)
                |> Maybe.withDefault (Err "symbol not found")
