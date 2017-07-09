module Env
    exposing
        ( debug
        , globalFrameId
        , global
        , push
        , pop
        , jump
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


debug : Env -> String -> a -> a
debug env msg value =
    if env.debug then
        Debug.log msg value
    else
        value


globalFrameId : Int
globalFrameId =
    0


defaultGcInterval : Int
defaultGcInterval =
    3


global : Env
global =
    { frames = Dict.singleton globalFrameId (emptyFrame Nothing)
    , nextFrameId = globalFrameId + 1
    , currentFrameId = globalFrameId
    , atoms = Dict.empty
    , nextAtomId = 0
    , debug = False
    , gcInterval = defaultGcInterval
    , gcCounter = 0
    }


getFrame : Int -> Env -> Frame
getFrame frameId env =
    case Dict.get frameId env.frames of
        Just frame ->
            frame

        Nothing ->
            Debug.crash <| "frame #" ++ (toString frameId) ++ " not found"


emptyFrame : Maybe Int -> Frame
emptyFrame outerId =
    { outerId = outerId
    , data = Dict.empty
    , refCnt = 1
    }


set : String -> MalExpr -> Env -> Env
set name expr env =
    let
        frameId =
            env.currentFrameId

        updateFrame =
            Maybe.map
                (\frame ->
                    { frame | data = Dict.insert name expr frame.data }
                )

        newFrames =
            Dict.update frameId updateFrame env.frames
    in
        { env | frames = newFrames }


get : String -> Env -> Result String MalExpr
get name env =
    let
        go frameId =
            let
                frame =
                    getFrame frameId env
            in
                case Dict.get name frame.data of
                    Just value ->
                        Ok value

                    Nothing ->
                        frame.outerId
                            |> Maybe.map go
                            |> Maybe.withDefault (Err <| "'" ++ name ++ "' not found")
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


jump : Int -> Env -> Env
jump frameId env =
    { env | currentFrameId = frameId }


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

        frame =
            getFrame frameId env
    in
        case frame.outerId of
            Just outerId ->
                { env
                    | currentFrameId = outerId
                    , frames = Dict.update frameId free env.frames
                }

            _ ->
                Debug.crash "tried to pop global frame"


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
            debug env "enter #" env.nextFrameId

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
            debug env "leave #" env.currentFrameId
    in
        { env
            | currentFrameId = orgFrameId
            , frames = Dict.update frameId free env.frames
        }


{-| Increase refCnt for the current frame,
and all it's parent frames.
-}
ref : Env -> Env
ref env =
    let
        go frameId env =
            let
                frame =
                    getFrame frameId env

                newFrame =
                    { frame | refCnt = frame.refCnt + 1 }

                newEnv =
                    { env | frames = Dict.insert frameId newFrame env.frames }
            in
                case frame.outerId of
                    Just outerId ->
                        go outerId newEnv

                    Nothing ->
                        newEnv

        newEnv =
            go env.currentFrameId env
    in
        { newEnv | gcCounter = newEnv.gcCounter + 1 }


free : Maybe Frame -> Maybe Frame
free =
    Maybe.andThen
        (\frame ->
            if frame.refCnt == 1 then
                Nothing
            else
                Just { frame | refCnt = frame.refCnt - 1 }
        )


{-| Given an Env see which frames are not reachable from the
global frame, or from the current expression.

Return a new Env with the unreachable frames removed.

-}
gc : MalExpr -> Env -> Env
gc currentExpr env =
    let
        countList acc =
            List.foldl countRefs acc

        countFrame { data } acc =
            data |> Dict.values |> countList acc

        recur frameId acc =
            if not (Set.member frameId acc) then
                let
                    frame =
                        getFrame frameId env

                    newAcc =
                        (Set.insert frameId acc)
                in
                    countFrame frame newAcc
            else
                acc

        countRefs expr acc =
            case expr of
                MalFunction (UserFunc { frameId }) ->
                    recur frameId acc

                MalApply { frameId } ->
                    recur frameId acc

                MalList list ->
                    countList acc list

                MalVector vec ->
                    countList acc (Array.toList vec)

                MalMap map ->
                    countList acc (Dict.values map)

                MalAtom atomId ->
                    let
                        value =
                            getAtom atomId env
                    in
                        countRefs value acc

                _ ->
                    acc

        initSet =
            Set.fromList [ globalFrameId, env.currentFrameId ]

        expandParents frameId acc =
            let
                frame =
                    getFrame frameId env
            in
                case frame.outerId of
                    Just parentId ->
                        Set.insert parentId acc

                    Nothing ->
                        acc

        expandAllFrames frames =
            Set.foldl expandParents frames frames

        makeEmptyFrame frameId =
            ( frameId, emptyFrame Nothing )

        globalFrame =
            getFrame globalFrameId env

        makeNewEnv newFrames =
            { env
                | frames = newFrames
                , gcCounter = 0
            }

        keepFilter keep frameId _ =
            Set.member frameId keep

        filterFrames frames keep =
            Dict.filter (keepFilter keep) frames
    in
        initSet
            |> countRefs currentExpr
            |> countFrame globalFrame
            |> expandAllFrames
            |> filterFrames env.frames
            |> makeNewEnv
