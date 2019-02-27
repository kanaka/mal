port module IO
    exposing
        ( IO(..)
        , writeLine
        , readLine
        , readFile
        , input
        , decodeIO
        )

import Json.Decode exposing (..)
import Time exposing (Time)


{-| Output a string to stdout
-}
port writeLine : String -> Cmd msg


{-| Read a line from the stdin
-}
port readLine : String -> Cmd msg


{-| Read the contents of a file
-}
port readFile : String -> Cmd msg


{-| Received a response for a command.
-}
port input : (Value -> msg) -> Sub msg


type IO
    = LineRead (Maybe String)
    | LineWritten
    | FileRead String
    | Exception String
    | GotTime Time


decodeIO : Decoder IO
decodeIO =
    field "tag" string
        |> andThen decodeTag


decodeTag : String -> Decoder IO
decodeTag tag =
    case tag of
        "lineRead" ->
            field "line" (nullable string)
                |> map LineRead

        "lineWritten" ->
            succeed LineWritten

        "fileRead" ->
            field "contents" string
                |> map FileRead

        "exception" ->
            field "message" string
                |> map Exception

        _ ->
            fail <|
                "Trying to decode IO, but tag "
                    ++ tag
                    ++ " is not supported."
