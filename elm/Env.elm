module Env exposing (global, push, pop, enter, leave, ref, get, set)

import Types exposing (MalExpr, Frame, Env)
import Dict exposing (Dict)


global : Env
global =
    { frames = Dict.singleton 0 (emptyFrame Nothing)
    , nextFrameId = 1
    , currentFrameId = 0
    }


push : Env -> Env
push env =
    let
        frameId =
            env.nextFrameId

        newFrame =
            emptyFrame (Just env.currentFrameId)
    in
        { currentFrameId = frameId
        , frames = Dict.insert frameId newFrame env.frames
        , nextFrameId = env.nextFrameId + 1
        }



-- TODO Dont' return result, Debug.crash instead.


pop : Env -> Result String Env
pop env =
    let
        frameId =
            env.currentFrameId
    in
        case Dict.get frameId env.frames of
            Just currentFrame ->
                case currentFrame.outerId of
                    Just outerId ->
                        Ok
                            { env
                                | currentFrameId = outerId
                                , frames = Dict.update frameId deref env.frames
                            }

                    Nothing ->
                        Err "tried to pop global frame"

            Nothing ->
                Err ("current frame " ++ (toString frameId) ++ " doesn't exist")


setBinds : List ( String, MalExpr ) -> Frame -> Frame
setBinds binds frame =
    case binds of
        [] ->
            frame

        ( name, expr ) :: rest ->
            setBinds rest
                { frame | data = Dict.insert name expr frame.data }


enter : Int -> List ( String, MalExpr ) -> Env -> Env
enter parentFrameId binds env =
    let
        frameId =
            env.nextFrameId

        newFrame =
            setBinds binds (emptyFrame (Just parentFrameId))
    in
        { currentFrameId = frameId
        , frames = Dict.insert frameId newFrame env.frames
        , nextFrameId = env.nextFrameId + 1
        }


leave : Int -> Env -> Env
leave orgFrameId env =
    let
        frameId =
            env.currentFrameId
    in
        { env
            | currentFrameId = orgFrameId
            , frames = Dict.update frameId deref env.frames
        }


{-| Increase refCnt for the current frame
-}
ref : Env -> Env
ref env =
    let
        incRef =
            Maybe.map
                (\frame ->
                    { frame | refCnt = frame.refCnt + 1 }
                )

        newFrames =
            Dict.update env.currentFrameId incRef env.frames
    in
        { env | frames = newFrames }



-- TODO: when disposing, deref all function's frames?
-- TODO: is that enough instead of a GC?


deref : Maybe Frame -> Maybe Frame
deref =
    Maybe.andThen
        (\frame ->
            if frame.refCnt == 1 then
                Nothing
            else
                Just { frame | refCnt = frame.refCnt - 1 }
        )



-- TODO need a GC.
-- given a Env, see which frames are not reachable.
-- in MalFunction need to refer to the frameId.


emptyFrame : Maybe Int -> Frame
emptyFrame outerId =
    { outerId = outerId
    , data = Dict.empty
    , refCnt = 1
    }


set : String -> MalExpr -> Env -> Env
set name expr env =
    let
        updateFrame =
            Maybe.map
                (\frame ->
                    { frame | data = Dict.insert name expr frame.data }
                )

        frameId =
            env.currentFrameId

        newFrames =
            Dict.update frameId updateFrame env.frames
    in
        { env | frames = newFrames }


get : String -> Env -> Result String MalExpr
get name env =
    let
        go frameId =
            case Dict.get frameId env.frames of
                Nothing ->
                    Err "frame not found"

                Just frame ->
                    case Dict.get name frame.data of
                        Just value ->
                            Ok value

                        Nothing ->
                            frame.outerId
                                |> Maybe.map go
                                |> Maybe.withDefault (Err "symbol not found")
    in
        go env.currentFrameId
