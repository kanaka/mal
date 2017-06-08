port module IO
    exposing
        ( IO(..)
        , writeLine
        , readLine
        , input
        , decodeIO
        )

import Json.Decode exposing (..)


{-| Output a string to stdout
-}
port writeLine : String -> Cmd msg


{-| Read a line from the stdin
-}
port readLine : String -> Cmd msg


{-| Received a response for a command.
-}
port input : (Value -> msg) -> Sub msg


type IO
    = LineRead (Maybe String)
    | LineWritten


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

        _ ->
            fail <|
                "Trying to decode IO, but tag "
                    ++ tag
                    ++ " is not supported."
