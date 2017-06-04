port module Main exposing (..)

import Platform exposing (programWithFlags)
import Json.Decode


port output : String -> Cmd msg


port readLine : String -> Cmd msg


port input : (Maybe String -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = \model -> input Input
        }


type alias Flags =
    { args : List String
    }


type alias Model =
    { args : List String
    }


type Msg
    = Input (Maybe String)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( flags, readLine prompt )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input (Just line) ->
            ( model
            , Cmd.batch
                [ output (rep line)
                , readLine prompt
                ]
            )

        Input Nothing ->
            ( model, Cmd.none )


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
