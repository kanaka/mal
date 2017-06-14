module Env
    exposing
        ( global
        , push
        , pop
        , enter
        , leave
        , ref
        , get
        , set
        , newAtom
        , getAtom
        , setAtom
        , gc
        )

import Types exposing (MalExpr(..), MalFunction(..), Frame, Env)
import Dict
import Array
import Set


globalFrameId : Int
globalFrameId =
    0


global : Env
global =
    { frames = Dict.singleton globalFrameId (emptyFrame Nothing)
    , nextFrameId = globalFrameId + 1
    , currentFrameId = globalFrameId
    , atoms = Dict.empty
    , nextAtomId = 0
    }


push : Env -> Env
push env =
    let
        frameId =
            env.nextFrameId

        newFrame =
            emptyFrame (Just env.currentFrameId)
    in
        { env
            | currentFrameId = frameId
            , frames = Dict.insert frameId newFrame env.frames
            , nextFrameId = env.nextFrameId + 1
        }


pop : Env -> Env
pop env =
    let
        frameId =
            env.currentFrameId
    in
        case Dict.get frameId env.frames of
            Just currentFrame ->
                case currentFrame.outerId of
                    Just outerId ->
                        { env
                            | currentFrameId = outerId
                            , frames = Dict.update frameId deref env.frames
                        }

                    _ ->
                        Debug.crash "tried to pop global frame"

            Nothing ->
                Debug.crash <|
                    "current frame "
                        ++ (toString frameId)
                        ++ " doesn't exist"


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
            Debug.log "enter #"
                env.nextFrameId

        newFrame =
            setBinds binds (emptyFrame (Just parentFrameId))
    in
        { env
            | currentFrameId = frameId
            , frames = Dict.insert frameId newFrame env.frames
            , nextFrameId = env.nextFrameId + 1
        }


leave : Int -> Env -> Env
leave orgFrameId env =
    let
        frameId =
            Debug.log "leave #"
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
-- TODO: is that enough instead of a GC? no: don't know how often the function is referenced.
-- TODO: consideration: keep refCnt for MalFunction, or implement a light GC.


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


{-| Given an Env see which frames are not reachable from the
global frame. Return a new Env without the unreachable frames.
-}
gc : Env -> Env
gc env =
    let
        countList acc =
            List.foldl countRefs acc

        countFrame acc { data } =
            data |> Dict.values |> countList acc

        countRefs expr acc =
            Debug.log (toString expr) <|
                case expr of
                    MalFunction (UserFunc { frameId }) ->
                        if not (Set.member frameId acc) then
                            Debug.log "counting" <|
                                case Dict.get frameId env.frames of
                                    Just frame ->
                                        countFrame (Set.insert frameId acc) frame

                                    Nothing ->
                                        Debug.crash ("frame " ++ (toString frameId) ++ " not found in GC")
                        else
                            acc

                    MalList list ->
                        countList acc list

                    MalVector vec ->
                        countList acc (Array.toList vec)

                    MalMap map ->
                        countList acc (Dict.values map)

                    _ ->
                        acc

        initSet =
            Set.fromList [ globalFrameId, env.currentFrameId ]
    in
        case Dict.get globalFrameId env.frames of
            Nothing ->
                Debug.crash "global frame not found"

            Just globalFrame ->
                countFrame initSet globalFrame
                    |> Set.toList
                    |> Debug.log "used frames"
                    |> List.map (\frameId -> ( frameId, emptyFrame Nothing ))
                    |> Dict.fromList
                    |> Dict.intersect (Debug.log "cur frames" env.frames)
                    |> (\frames -> { env | frames = frames })


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
                    Err <| "frame " ++ (toString frameId) ++ " not found"

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


newAtom : MalExpr -> Env -> ( Env, Int )
newAtom value env =
    let
        atomId =
            env.nextAtomId

        newEnv =
            { env
                | atoms = Dict.insert atomId value env.atoms
                , nextAtomId = atomId + 1
            }
    in
        ( newEnv, atomId )


getAtom : Int -> Env -> MalExpr
getAtom atomId env =
    case Dict.get atomId env.atoms of
        Just value ->
            value

        Nothing ->
            Debug.crash <| "atom " ++ (toString atomId) ++ " not found"


setAtom : Int -> MalExpr -> Env -> Env
setAtom atomId value env =
    { env
        | atoms = Dict.insert atomId value env.atoms
    }
