port module Main exposing (..)

import IO exposing (..)
import Json.Decode exposing (decodeValue)
import Platform exposing (programWithFlags)
import Types exposing (MalExpr(..))
import Reader exposing (readString)
import Printer exposing (printStr)


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


type alias Model =
    { args : List String
    }


type Msg
    = Input (Result String IO)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( flags, readLine prompt )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input (Ok (LineRead (Just line))) ->
            case rep line of
                Just out ->
                    ( model, writeLine out )

                Nothing ->
                    ( model, readLine prompt )

        Input (Ok LineWritten) ->
            ( model, readLine prompt )

        Input (Ok (LineRead Nothing)) ->
            ( model, Cmd.none )

        Input (Ok io) ->
            Debug.crash "unexpected IO received: " io

        Input (Err msg) ->
            Debug.crash msg ( model, Cmd.none )


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


eval : MalExpr -> MalExpr
eval ast =
    ast


print : MalExpr -> String
print =
    printStr True


{-| Read-Eval-Print
-}
rep : String -> Maybe String
rep =
    let
        formatResult result =
            case result of
                Ok optStr ->
                    optStr

                Err msg ->
                    Just msg
    in
        readString
            >> Result.map (Maybe.map (eval >> print))
            >> formatResult
