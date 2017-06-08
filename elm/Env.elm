module Env exposing (make, set, get)

import Types exposing (MalExpr(..), Env(..))
import Dict exposing (Dict)


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
