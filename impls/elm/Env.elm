module Env
    exposing
        ( debug
        , globalFrameId
        , global
        , get
        , set
        , newAtom
        , getAtom
        , setAtom
        , push
        , pop
        , enter
        , leave
        , ref
        , pushRef
        , restoreRefs
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
    10


global : Env
global =
    { frames = Dict.singleton globalFrameId (emptyFrame Nothing Nothing)
    , nextFrameId = globalFrameId + 1
    , currentFrameId = globalFrameId
    , atoms = Dict.empty
    , nextAtomId = 0
    , debug = False
    , gcInterval = defaultGcInterval
    , gcCounter = 0
    , stack = []
    , keepFrames = []
    }


getFrame : Env -> Int -> Frame
getFrame env frameId =
    case Dict.get frameId env.frames of
        Just frame ->
            frame

        Nothing ->
            Debug.crash <| "frame #" ++ (toString frameId) ++ " not found"


emptyFrame : Maybe Int -> Maybe Int -> Frame
emptyFrame outerId exitId =
    { outerId = outerId
    , exitId = exitId
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
                    getFrame env frameId
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


push : Env -> Env
push env =
    let
        frameId =
            env.nextFrameId

        newFrame =
            emptyFrame (Just env.currentFrameId) Nothing

        bogus =
            debug env "push" frameId
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
            getFrame env frameId

        bogus =
            debug env "pop" frameId
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


{-| Enter a new frame with a set of binds
-}
enter : Int -> List ( String, MalExpr ) -> Env -> Env
enter outerId binds env =
    let
        frameId =
            debug env "enter #" env.nextFrameId

        exitId =
            env.currentFrameId

        newFrame =
            setBinds binds (emptyFrame (Just outerId) (Just exitId))
    in
        { env
            | currentFrameId = frameId
            , frames = Dict.insert frameId newFrame env.frames
            , nextFrameId = env.nextFrameId + 1
        }


leave : Env -> Env
leave env =
    let
        frameId =
            debug env "leave #" env.currentFrameId

        frame =
            getFrame env frameId

        exitId =
            case frame.exitId of
                Just exitId ->
                    exitId

                Nothing ->
                    Debug.crash <|
                        "frame #"
                            ++ (toString frameId)
                            ++ " doesn't have an exitId"
    in
        { env
            | currentFrameId = exitId
            , frames =
                env.frames
                    |> Dict.insert frameId { frame | exitId = Nothing }
                    |> Dict.update frameId free
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
                    getFrame env frameId

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


pushRef : MalExpr -> Env -> Env
pushRef ref env =
    { env | stack = ref :: env.stack }


restoreRefs : List MalExpr -> Env -> Env
restoreRefs refs env =
    { env | stack = refs }


{-| Given an Env see which frames are not reachable from the
global frame, or from the current expression.

Return a new Env with the unreachable frames removed.

-}
gc : MalExpr -> Env -> Env
gc expr env =
    let
        countList acc =
            List.foldl countExpr acc

        countFrame { data } acc =
            data |> Dict.values |> countList acc

        recur frameId acc =
            if not (Set.member frameId acc) then
                let
                    frame =
                        getFrame env frameId

                    newAcc =
                        Set.insert frameId acc
                in
                    countFrame frame newAcc
            else
                acc

        countBound bound acc =
            bound
                |> List.map Tuple.second
                |> countList acc

        countExpr expr acc =
            case expr of
                MalFunction (UserFunc { frameId }) ->
                    recur frameId acc

                MalApply { frameId, bound } ->
                    recur frameId acc
                        |> countBound bound

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
                        countExpr value acc

                _ ->
                    acc

        initSet =
            Set.fromList
                ([ globalFrameId, env.currentFrameId ]
                    ++ env.keepFrames
                )

        countFrames frames acc =
            Set.toList frames
                |> List.map (getFrame env)
                |> List.foldl countFrame acc

        expand frameId frame fn acc =
            case fn frame of
                Nothing ->
                    acc

                Just parentId ->
                    Set.insert parentId acc

        expandBoth frameId =
            let
                frame =
                    getFrame env frameId
            in
                expand frameId frame .outerId
                    >> expand frameId frame .exitId

        expandParents frames =
            Set.foldl expandBoth frames frames

        loop acc =
            let
                newAcc =
                    expandParents acc

                newParents =
                    Set.diff newAcc acc
            in
                if Set.isEmpty newParents then
                    newAcc
                else
                    loop <| countFrames newParents newAcc

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
        countFrames initSet initSet
            |> countExpr expr
            |> (flip countList) env.stack
            |> loop
            |> filterFrames env.frames
            |> makeNewEnv
