module Step1_read_print exposing (..)

import IO exposing (..)
import Json.Decode exposing (decodeValue, errorToString)
import Types exposing (MalExpr(..))
import Reader exposing (readString)
import Printer exposing (printStr)


main : Program Flags Model Msg
main =
    Platform.worker
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

        Input (Ok io) ->
            Debug.todo "unexpected IO received: " io

        Input (Err msg2) ->
            Debug.todo msg2 ( model, Cmd.none )


prompt : String
prompt =
    "user> "


{-| read can return three things:

Ok (Just expr) -> parsed okay
Ok Nothing -> empty string (only whitespace and/or comments)
Err msg -> parse error

-}
read : String -> Result String MalExpr
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
rep : String -> String
rep =
    let
        formatResult result =
            case result of
                Ok optStr ->
                    optStr

                Err msg ->
                    msg
    in
        readString
            >> Result.map (eval >> print)
            >> formatResult
