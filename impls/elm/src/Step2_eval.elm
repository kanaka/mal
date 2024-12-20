module Step2_eval exposing (..)

import Array
import Dict exposing (Dict)
import Eval
import IO exposing (..)
import Json.Decode exposing (decodeValue, errorToString)
import Platform exposing (worker)
import Printer exposing (printStr)
import Reader exposing (readString)
import Tuple exposing (mapFirst, second)
import Types exposing (..)
import Utils exposing (maybeToList, zip)


main : Program Flags Model Msg
main =
    worker
        { init = init
        , update = update
        , subscriptions =
            \model -> input (decodeValue decodeIO >> (\x -> case x of
                Err e -> Err (errorToString e)
                Ok a  -> Ok a
            ) >>  Input)
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
            CoreFunc Nothing >> MalFunction

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
            let ( result, newEnv) = rep model.env line
            in ( { model | env = newEnv }, writeLine (makeOutput result) )

        Input (Ok LineWritten) ->
            ( model, readLine prompt )

        Input (Ok (LineRead Nothing)) ->
            ( model, Cmd.none )

        Input (Ok io) ->
            Debug.todo "unexpected IO received: " io

        Input (Err msg2) ->
            Debug.todo msg2 ( model, Cmd.none )


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


read : String -> Result String MalExpr
read =
    readString


eval : ReplEnv -> MalExpr -> ( Result String MalExpr, ReplEnv )
eval env ast =
  -- let
  --   _ = Debug.log ("EVAL: " ++ printStr env True ast) ()
  --       --  The output ends with an ugly ": ()", but that does not hurt.
  -- in
    case ast of
        MalList _ [] ->
            ( Ok ast, env )

        MalList _ list ->
            case evalList env list [] of
                ( Ok newList, newEnv ) ->
                    case newList of
                        [] ->
                            ( Err "can't happen", newEnv )

                        (MalFunction (CoreFunc _ fn)) :: args ->
                            case Eval.runSimple (fn args) of
                                Ok res ->
                                    ( Ok res, newEnv )

                                Err msg ->
                                    ( Err (print msg), newEnv )

                        fn :: _ ->
                            ( Err ((print fn) ++ " is not a function"), newEnv )

                ( Err msg, newEnv ) ->
                    ( Err msg, newEnv )

        MalSymbol sym ->
            -- Lookup symbol in env and return value or raise error if not found.
            case Dict.get sym env of
                Just val ->
                    ( Ok val, env )

                Nothing ->
                    ( Err ("symbol '" ++ sym ++ "' not found"), env )

        MalVector _ vec ->
            evalList env (Array.toList vec) []
                |> mapFirst (Result.map (Array.fromList >> MalVector Nothing))

        MalMap _ map ->
            evalList env (Dict.values map) []
                |> mapFirst
                    (Result.map
                        (zip (Dict.keys map)
                            >> Dict.fromList
                            >> MalMap Nothing
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


{-| Read-Eval-Print
-}
rep : ReplEnv -> String -> ( Result String String, ReplEnv )
rep env input =
    let
        evalPrint =
            eval env >> mapFirst (Result.map print)
    in
    case readString input of
        Err msg ->
            ( Err msg, env )

        Ok ast ->
            evalPrint ast
