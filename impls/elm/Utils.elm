module Utils
    exposing
        ( decodeString
        , encodeString
        , makeCall
        , wrap
        , maybeToList
        , zip
        , last
        , justValues
        )

import Regex exposing (replace)
import Types exposing (MalExpr(..))


regex str = case Regex.fromString str of
    Nothing -> Debug.todo "invalid regex"
    Just r  -> r

decodeString : String -> String
decodeString =
    let
        unescape { match } =
            case match of
                "\\n" ->
                    "\n"

                "\\\"" ->
                    "\""

                "\\\\" ->
                    "\\"

                other ->
                    other
    in
        String.slice 1 -1
            >> replace (regex "\\\\[\\\"\\\\n]") unescape


encodeString : String -> String
encodeString =
    let
        escape { match } =
            case match of
                "\n" ->
                    "\\n"

                "\"" ->
                    "\\\""

                "\\" ->
                    "\\\\"

                other ->
                    other
    in
        wrap "\"" "\""
            << replace (regex "[\\n\\\"\\\\]") escape


makeCall : String -> List MalExpr -> MalExpr
makeCall symbol args =
    MalList Nothing <| (MalSymbol symbol) :: args


wrap : String -> String -> String -> String
wrap prefix suffix str =
    prefix ++ str ++ suffix


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Just x ->
            [ x ]

        Nothing ->
            []


zip : List a -> List b -> List ( a, b )
zip a b =
    case ( a, b ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            last xs


justValues : List (Maybe a) -> List a
justValues list =
    case list of
        [] ->
            []

        (Just x) :: rest ->
            x :: (justValues rest)

        Nothing :: rest ->
            justValues rest
