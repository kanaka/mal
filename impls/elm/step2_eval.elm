port module Main exposing (..)

import IO exposing (..)
import Json.Decode exposing (decodeValue)
import Platform exposing (programWithFlags)
import Types exposing (..)
import Reader exposing (readString)
import Printer exposing (printStr)
import Utils exposing (maybeToList, zip)
import Dict exposing (Dict)
import Tuple exposing (mapFirst, second)
import Array
import Eval


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions =
            \model -> input (decodeValue decodeIO >> Input)
        }


type alias Flags =
    { args : List String
    }


type alias ReplEnv =
    Dict String MalExpr


type alias Model =
    { args : List String
    , env : ReplEnv
    }


type Msg
    = Input (Result String IO)


init : Flags -> ( Model, Cmd Msg )
init { args } =
    ( { args = args, env = initReplEnv }, readLine prompt )


initReplEnv : ReplEnv
initReplEnv =
    let
        makeFn =
            CoreFunc >> MalFunction

        binaryOp fn args =
            case args of
                [ MalInt x, MalInt y ] ->
                    Eval.succeed <| MalInt (fn x y)

                _ ->
                    Eval.fail "unsupported arguments"
    in
        Dict.fromList
            [ ( "+", makeFn <| binaryOp (+) )
            , ( "-", makeFn <| binaryOp (-) )
            , ( "*", makeFn <| binaryOp (*) )
            , ( "/", makeFn <| binaryOp (//) )
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input (Ok (LineRead (Just line))) ->
            case rep model.env line of
                Nothing ->
                    ( model, readLine prompt )

                Just ( result, newEnv ) ->
                    ( { model | env = newEnv }, writeLine (makeOutput result) )

        Input (Ok LineWritten) ->
            ( model, readLine prompt )

        Input (Ok (LineRead Nothing)) ->
            ( model, Cmd.none )

        Input (Ok io) ->
            Debug.crash "unexpected IO received: " io

        Input (Err msg) ->
            Debug.crash msg ( model, Cmd.none )


makeOutput : Result String String -> String
makeOutput result =
    case result of
        Ok str ->
            str

        Err msg ->
            "Error: " ++ msg


prompt : String
prompt =
    "user> "


{-| read can return three things:

Ok (Just expr) -> parsed okay
Ok Nothing -> empty string (only whitespace and/or comments)
Err msg -> parse error

-}
read : String -> Result String (Maybe MalExpr)
read =
    readString


eval : ReplEnv -> MalExpr -> ( Result String MalExpr, ReplEnv )
eval env ast =
    case ast of
        MalList [] ->
            ( Ok ast, env )

        MalList list ->
            case evalList env list [] of
                ( Ok newList, newEnv ) ->
                    case newList of
                        [] ->
                            ( Err "can't happen", newEnv )

                        (MalFunction (CoreFunc fn)) :: args ->
                            case Eval.runSimple (fn args) of
                                Ok res ->
                                    ( Ok res, newEnv )

                                Err msg ->
                                    ( Err (print msg), newEnv )

                        fn :: _ ->
                            ( Err ((print fn) ++ " is not a function"), newEnv )

                ( Err msg, newEnv ) ->
                    ( Err msg, newEnv )

        _ ->
            evalAst env ast


evalAst : ReplEnv -> MalExpr -> ( Result String MalExpr, ReplEnv )
evalAst env ast =
    case ast of
        MalSymbol sym ->
            -- Lookup symbol in env and return value or raise error if not found.
            case Dict.get sym env of
                Just val ->
                    ( Ok val, env )

                Nothing ->
                    ( Err "symbol not found", env )

        MalList list ->
            -- Return new list that is result of calling eval on each element of list.
            evalList env list []
                |> mapFirst (Result.map MalList)

        MalVector vec ->
            evalList env (Array.toList vec) []
                |> mapFirst (Result.map (Array.fromList >> MalVector))

        MalMap map ->
            evalList env (Dict.values map) []
                |> mapFirst
                    (Result.map
                        (zip (Dict.keys map)
                            >> Dict.fromList
                            >> MalMap
                        )
                    )

        _ ->
            ( Ok ast, env )


evalList : ReplEnv -> List MalExpr -> List MalExpr -> ( Result String (List MalExpr), ReplEnv )
evalList env list acc =
    case list of
        [] ->
            ( Ok (List.reverse acc), env )

        x :: rest ->
            case eval env x of
                ( Ok val, newEnv ) ->
                    evalList newEnv rest (val :: acc)

                ( Err msg, newEnv ) ->
                    ( Err msg, newEnv )


{-| Try to map a list with a fn that can return a Err.

Maps the list from left to right. As soon as a error
occurs it will not process any more elements and return
the error.

-}
tryMapList : (a -> Result e b) -> List a -> Result e (List b)
tryMapList fn list =
    let
        go x =
            Result.andThen
                (\acc ->
                    case fn x of
                        Ok val ->
                            Ok (val :: acc)

                        Err msg ->
                            Err msg
                )
    in
        List.foldl go (Ok []) list
            |> Result.map List.reverse


print : MalExpr -> String
print =
    printStr True


{-| Read-Eval-Print. rep returns:

Nothing -> if an empty string is read (ws/comments)
Just ((Ok out), newEnv) -> input has been evaluated.
Just ((Err msg), env) -> error parsing or evaluating.

-}
rep : ReplEnv -> String -> Maybe ( Result String String, ReplEnv )
rep env input =
    let
        evalPrint =
            eval env >> mapFirst (Result.map print)
    in
        case readString input of
            Ok Nothing ->
                Nothing

            Err msg ->
                Just ( Err msg, env )

            Ok (Just ast) ->
                Just (evalPrint ast)
