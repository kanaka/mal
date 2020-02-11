port module Main exposing (..)

import IO exposing (..)
import Json.Decode exposing (decodeValue)
import Platform exposing (programWithFlags)


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
            ( model, writeLine (rep line) )

        Input (Ok LineWritten) ->
            ( model, readLine prompt )

        Input (Ok (LineRead Nothing)) ->
            ( model, Cmd.none )

        Input (Ok _) ->
            ( model, Cmd.none )

        Input (Err msg) ->
            Debug.crash msg ( model, Cmd.none )


prompt : String
prompt =
    "user> "


read : String -> String
read ast =
    ast


eval : String -> String
eval ast =
    ast


print : String -> String
print ast =
    ast


rep : String -> String
rep =
    read >> eval >> print
